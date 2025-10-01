;; ===============================================================================================================
;; Generate documentation from SKILL, SKILL++ or SKILL# files
;;
;; A. Buchet - June 2025
;; ===============================================================================================================

;; =======================================================
;; Fetch global definitions
;; =======================================================

(let ()

  (@fun @globals
    ( @key
      ( files
        ?type ( string ... )
        ?doc  "List of files from which the global definitions are reported."
        )
      ( show_props
        ?type t|nil
        ?def  (equal "TRUE" (getShellEnvVar "SKILL_SHARP_GLOBALS_SHOW_PROPS"))
        ?doc  "If non-nil modified symbol properties are also reported."
        )
      ( load_files
        ?type t|nil
        ?def  (equal "TRUE" (getShellEnvVar "SKILL_SHARP_GLOBALS_LOAD"))
        ?doc  "If non-nil, files are loaded in an independent SKILL process :
  BEFORE command is run.
  The whole SKILL environment is cached.
  FILES are loaded.
  INIT command is run.
  All discrepancies betwwen cached and current environments are reported."
        )
      ( before
        ?type string
        ?def  (or (getShellEnvVar "SKILL_SHARP_BEFORE_COMMAND") "")
        ?doc  "When LOAD is non-nil, this command is executed before loading FILES."
        )
      ( init
        ?type string
        ?def  (or (getShellEnvVar "SKILL_SHARP_INIT_COMMAND"  ) "")
        ?doc  "When LOAD is non-nil, this command is executed after loading FILES."
        )
      @rest _
      )
    ?doc "Return all global definitions from FILES."
    ?out ( ( symbol ... ) ... )
    ?global t
    (assert files "@globals - ?files is nil")
    (if load_files
        ;; Load files and report definitions using `globals`
        (destructuringBind ( stdout stderr _status )
                           (@bash (@str "
  export SKILL_SHARP_BEFORE_COMMAND=\"{(escape_quotes before)}\";
  export SKILL_SHARP_INIT_COMMAND=\"{(escape_quotes init)}\";
  export SKILL_SHARP_GLOBALS_SHOW_PROPS=\"{(if show_props 'TRUE 'FALSE)}\";
  $SKILL_SHARP_ROOT/bin/globals {(buildString files)}"))
          (unless (blankstrp stderr) (warn "Warning/Error when running `globals`: %s" stderr))
          (foreach mapcar str (parseString stdout "\n") (car (linereadstring str)))
          );dbind
      ;; No load, use Lint to parse the files and report global definitions
      (@with ( ( port     (outstring)           )
               ( nullport (outfile "/dev/null") )
               )
        (@lint
          ?files     files
          ?filters   '(GLOBAL)
          ?info_port port
          ?warn_port port
          ?err_port  nullport
          ?no_header t
          )
        (@letf ( ( (rexMagic) t )
                 )
          ;; Parse Lint results
          (let ( functions variables scheme classes symbols name )
            (foreach line (parseString (getOutstring port) "\n")
              (assert (pcreMatchp "`([a-zA-Z0-9_@\\\\]+)` global (scheme |function )?definition: ([a-zA-Z0-9_@?\\\\]+)" line) "Global message has the wrong format: %N" line)
              (setq name (concat (pcreSubstitute "\\3")))
              (@caseq (concat (pcreSubstitute "\\1"))
                ( (define setq )
                  (case (pcreSubstitute "\\2")
                    ( "scheme "   (push name scheme   ) )
                    ( "function " (push name functions) )
                    ( t           (push name variables) )
                    ) )
                ( putpropqq (push name symbols) )
                ( ( \\\@fun @fun defun defglobalfun defmethod defmacro ) (push name functions) )
                ));foreach line
            ;; Report global symbol properties only when required
            (if show_props
                (list functions variables scheme classes symbols )
              (list functions variables scheme classes)
              ));if ;let
          ));letf ;with
      ));if ;fun

  (@fun escape_quotes ( ( str ?type string ) )
    ?doc "Escape quotes inside STR and return it."
    ?out string
    (@exact_replace "\"" str "\\\"")
    )

  );closure

;; =======================================================
;; Generate .fnd documentation
;; =======================================================

(let ()

  ;; TODO - In docgen, we were cleaning backslashes, it might be simpler to add them only when necessary

  (@fun escape
    ( ( str ?type string )
      )
    ?doc "Return STR where special characters are escaped."
    ?out string
    ;; Double backslashes then escape in-string double-quotes
    (@exact_replace "\"" (buildString (parseString str "\\" t) "\\\\") "\\\"" 0)
    )

  (@fun clean_at_sign
    ( ( str ?type string )
      )
    ?doc "Remove backslash before 'at sign' when found at the beginning of names in STR."
    (pcreReplace (pcreCompile "\\B\\\\@") str "@" 0)
    )

  (@test
    ?fun 'clean_at_sign

    (@assertion
      ?doc "Simple test"
      (clean_at_sign "_fun_name_example")
      ?out "_fun_name_example"
      )

    (@assertion
      ?doc "\\@ should not be replaced inside names."
      (clean_at_sign "Example _\\@str.")
      ?out "Example _\\@str."
      )

    (@assertion
      ?doc "\\@ should not be replaced at beginning of names."
      (clean_at_sign "Example \\@alphalessp.")
      ?out "Example @alphalessp."
      )

    );test

  (@fun title
    ( ( name ?type symbol )
      )
    ?doc "Return proper title of function named NAME."
    ?out string
    ;; Replace '\@' by '@' but only at the beginning of name
    (escape (clean_at_sign (@to_string name)))
    )

  (@test
    ?fun 'title

    (@assertion
      ?doc "Simple test"
      (title '_fun_name_example)
      ?out "_fun_name_example"
      )

    (@assertion
      ?doc "Make sure special characters are well escaped."
      (title '_\@str)
      ?out "_@str"
      ;; TODO - Test is waived for now
      ;?out "_\\\\@str"
      )

    (@assertion
      ?doc "Make sure 'at sign' at beginning of symbol is not escaped."
      (title '@alphalessp)
      ?out "@alphalessp"
      )

    );test

  (@fun syntax
    ( ( name ?type symbol )
      )
    ?doc "Return syntax string of function named NAME."
    ?out string
    (@with ( ( port (outstring) )
             )
      (fprintf port "%s(" (title name))
      (@when (@arglist name)
        ?var args
        (fprintf port "<table>")
        (let ( (name_prefix "")
               (name_suffix "")
               )
          (foreach arg args
            ;; Support for _ as rest argument
            (when (eq '_ arg) (setq arg (list "" '?type '( any ... ) '?def nil)))
            (@caseq (type arg)
              ( symbol
                (@caseq arg
                  ( @key
                    (setq name_prefix "?")
                    (setq name_suffix "" )
                    )
                  ( @rest
                    (setq name_prefix ""    )
                    (setq name_suffix "..." )
                    )
                  ));caseq ;symbol
              ( list
                ;; Newlines are avoided in syntax strings, otherwise HTML table cells appear too high
                (destructuringBind ( name
                                     @key
                                     (type   '__undefined__                      )
                                     (doc    ""                                  )
                                     (def    '__undefined__                      )
                                     ) arg
                  (let ( ( open  (if (eq '__undefined__ def) "" "[ ") )
                         ( close (if (eq '__undefined__ def) "" " ]") )
                         )
                    ;; Update name, type doc and def when necessary
                    (setq name (escape (lsprintf "%s%s%s" name_prefix name name_suffix)        ))
                    (setq type (escape (if (eq '__undefined__ type) "" (@pretty_print type t)) ))
                    (setq def  (escape (if (eq '__undefined__ def ) "" (@pretty_print def   )) ))
                    (setq doc  (escape (if (@nonblankstring? doc) (lsprintf " ; %s" doc) ""  ) ))
                    ;; First empty cell for indentation
                    (fprintf port "<tr><td></td>")
                    ;; Print brackets when argument has default value,
                    ;; question mark when key argument and three dots for rest one
                    (let ( ( name_color "black"      )
                           ( type_color "dimgrey"    )
                           ( def_color  "darkblue"   )
                           ( doc_color  "darkgreen"  )
                           )
                      (@fprintf port "\
<td style='color:{name_color};'><pre>{open  }</pre></td>\
<td style='color:{name_color};'><pre>{name } </pre></td>\
<td style='color:{type_color};'><pre>{type } </pre></td>\
<td style='color:{def_color };'><pre>{def  } </pre></td>\
<td style='color:{name_color};'><pre>{close }</pre></td>\
<td style='color:{doc_color };'><pre>{doc  } </pre></td>\
"))
                    (fprintf port "</tr>")
                    )));let ;destructuringBind ;list
              ));caseq ;foreach
        (fprintf port "</table>")
        ));let ;when
      (fprintf port ")")
      ;; Print output when defined
      (when (memq '@output (get name '?)) (fprintf port " => %s" (@pretty_print (get name '@output) t)))
      (getOutstring port)
      ));with ;fun

  (@fun abstract
    ( ( name ?type symbol )
      )
    ?doc "Return abstract string of function named NAME."
    ?out string
    (@with ( ( port (outstring) )
             )
      ;; Write function docstring when available
      (@if (fdoc name)
           ?var doc
           (fprintf port "%s" (escape doc))
         (fprintf port "%s" (escape (@str "Missing documentation for function `{(title name)}'.")))
         )
      ;; Write function tests when available
      (@when (name->@test)
        ?var test
        (fprintf port "\n<hr><pre>")
        (when (@nonblankstring? test->doc)
          (fprintf port "<font color='darkgreen'>;; %s</font>\n" test->doc)
          )
        (foreach map assertions (@get_assertions test)
          (@when (@is? '@nonblankstring? (car assertions)->doc)
            ?var doc
            (fprintf port "<font color='darkgreen'>;; %s</font>\n" doc)
            )
          ;; Print info, warn and error messages when expected
          (letseq ( ( assertion (car assertions)                                   )
                    ( input     assertion->body_quoted                             )
                    ( output    (car assertion->body_result)                       )
                    ( info      (@is? '@nonblankstring? assertion->info_expected ) )
                    ( warn      (@is? '@nonblankstring? assertion->warn_expected ) )
                    ( error     (@is? '@nonblankstring? assertion->error_expected) )
                    )
            ;; Shape input so it can be copy-pasted and run in CIW by any user
            (when (listp input)
              (if (cdr input)
                  (push 'progn input)
                (setq input (car input))
                ))
            (fprintf port "%s\n"    (escape (clean_at_sign (@pretty_print input  ))))
            (foreach (str prefix color) (list info warn error)
                                        '( "INFO" "WARNING" "ERROR" )
                                        '( "darkblue" "darkorange" "darkred" )
              (when str
                (foreach line (parseString str "\n")
                  (fprintf port "<font color='%s'>;%s>%s</font>\n" color prefix (escape (clean_at_sign line)))
                  ));when ;foreach
              );foreach
            (fprintf port ";> %s\n" (escape (clean_at_sign (@pretty_print output ))))
            (when (cdr assertions) (newline port))
            ));let ;foreach
        (fprintf port "</pre>")
        );when
      (getOutstring port)
      ));with ;fun

  (@fun @docgen
    ( @key
      ( files        ?type ( string ... )                                                                 )
      ( init         ?type string         ?def (or (getShellEnvVar "SKILL_SHARP_INIT_COMMAND"  ) "")      )
      ( before       ?type string         ?def (or (getShellEnvVar "SKILL_SHARP_BEFORE_COMMAND") "")      )
      ( track_source
        ?type t|nil
        ?def  (equal "TRUE" (getShellEnvVar "SKILL_SHARP_TRACK_SOURCE"))
        ?doc  "If non-nil, function source file is added as third string in .fnd list."
        )
      ( relative_var
        ?type string
        ?def  "$SKILL_SHARP_ROOT"
        ?doc  "If ?track_source is non-nil, try to use provided variable to make source path relative."
        )
      @rest _
      )
    ?doc   "Load all SKILL or SKILL++ FILES.
Print associated documentation (as .fnd file content) to stdout."
    ?out    t|nil
    ?global t
    (destructuringBind ( functions _variables _scheme _classes @optional _symbols )
                       (@globals ?files files ?before (@str "(progn nil (inSkill (sklint)) {before})") ?init init)
      ;; Load all files containing tests
      (@letf ( ( (status         keepNLInString        ) t      )
               ( (status         saveInlineDoc         ) t      )
               ( (getShellEnvVar "SKILL_SHARP_RUN_TEST") "TRUE" )
               )
        (foreach file files (@load file ?no_reload t))
        )
      ;; Filter and sort functions
      (setq functions (setof function functions (and (getd function) (nequal "_" (substring function 1 1)))))
      (setq functions (@sort functions ?comp '@alphalessp))
      (foreach function functions
        (let ( ( source ""                  )
               ( file   function->_loadFile )
               )
          (when (and track_source (stringp file))
            (setq source (strcat "\"" (@exact_replace (@realpath relative_var) (@realpath file) (strcat relative_var "/")) "\"\n  "))
            );when
          (@fprintf (@poport) "\
( \"{(title    function)}\"\n\
  \"{(syntax   function)}\"\n\
  \"{(abstract function)}\"\n\
  {source})\n"
            );@fprintf
          ));let ;foreach function
      ));dbind ;fun

  );closure

;; =======================================================
;; Check .fnd documentation
;; =======================================================

(let ()

  (@fun valid_sexp?
    ( ( sexp ?type any )
      )
    ?doc "Return t if SEXP is a valid .fnd one, nil otherwise.
A valid .fnd expression should be t or a list containing three strings."
    ?out t|nil
    (prog ()
      ;; `lineread' output should be a list of one element
      (cond
        ( (eq t sexp)
          (return t)
          )
        ( (not (listp sexp))
          (warn "Not a list: %N" sexp)
          (return)
          )
        ( (not (listp (car sexp)))
          (warn "First element is not a list: %N" sexp)
          (return)
          )
        ( (cdr sexp)
          (warn "List contains more than one element: %N" sexp)
          (return)
          )
        ( t
          (setq sexp (car sexp))
          )
        );cond
      ;; .fnd sexp should be a list of three strings
      (cond
        ( (not (listp sexp))
          (warn "Not a list: %N" sexp)
          (return)
          )
        ( (not (stringp (car sexp)))
          (warn "First element is not a string: %N" sexp)
          (return)
          )
        ( (not (stringp (cadr sexp)))
          (warn "Second element is not a string: %N" sexp)
          (return)
          )
        ( (not (stringp (caddr sexp)))
          (warn "Third element is not a string: %N" sexp)
          (return)
          )
        ;; In custom .fnd files, function source file is used as fourth element
        ; ( (cdddr sexp)
        ;   (warn "List contains more than three elements: %N" sexp)
        ;   (return)
        ;   )
        ( (cddddr sexp)
          (warn "List contains more than four elements: %N" sexp)
          (return)
          )
        ( t
          (return t)
          )
        );cond
    ));and ;fun

  (@fun @fndcheck
    ( @key ( files ?type ( string ... ) )
      @rest _
      )
    ?doc   "Make sure .fnd FILES are valid."
    ?out    t|nil
    ?global t
    (assert files "@fndcheck - no files were provided...")
    (forall file files
      (@with ( ( port (infile file))
               ;; This was used to match native Finder behavior which probably uses a different interpreter.
               ;; As it seems OK with meaningless escaped characters.
               ;( port  (instring (@exact_replace "\\@" (@file_contents file) "\\\\@")) )
               )
        (prog ( sexp )
          (while (car (setq sexp (errset (lineread port) t)))
            (unless (valid_sexp? (car sexp)) (return))
            );while
          (unless sexp
            (warn "Error when reading .fnd file")
            (return)
            )
          (and (car sexp) (not (valid_sexp? (car sexp))) (return))
          (return t)
          ));prog ;with
      ));forall ;fun

  )

;*/

