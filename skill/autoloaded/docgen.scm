;; ===============================================================================================================
;; Generate documentation from SKILL, SKILL++ or SKILL# files
;;
;; A. Buchet - June 2025
;; ===============================================================================================================

;; =======================================================
;; Fetch global definitions
;; =======================================================

(@fun @globals
  ( @key ( files  ?type ( string ... )                                                            )
         ( init   ?type string         ?def (or (getShellEnvVar "SKILL_SHARP_INIT_COMMAND"  ) "") )
         ( before ?type string         ?def (or (getShellEnvVar "SKILL_SHARP_BEFORE_COMMAND") "") )
    @rest _
    )
  ?doc "Return all global definitions from after loading FILES and run INIT."
  ?out ( ( symbol ... ) ... )
  (destructuringBind ( stdout stderr _status )
                     (@bash (@str "
export SKILL_SHARP_BEFORE_COMMAND=\"{before}\";
export SKILL_SHARP_INIT_COMMAND=\"{init}\";
$SKILL_SHARP_ROOT/bin/globals {(buildString files)}"))
    (unless (blankstrp stderr) (warn "Error when running `globals`: %s" stderr))
    (foreach mapcar str (parseString stdout "\n") (car (linereadstring str)))
    ))

;; =======================================================
;; Generate .fnd documentation
;; =======================================================

(let ()

  (@fun title
    ( ( name ?type symbol )
      )
    ?doc "Return proper title of function named NAME."
    ?out string
    (@escape_chars (pcreReplace (pcreCompile "^\\\\@") (@to_string name) "@" 0))
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
      ?out "_\\@str"
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
            (when (eq '_ arg) (setq arg (list "" ?type '( any ... ) ?def nil)))
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
                    (setq name (@escape_chars (lsprintf "%s%s%s" name_prefix name name_suffix))      )
                    (setq type (@escape_chars (if (eq '__undefined__ type) "" (lsprintf "%N" type))) )
                    (setq def  (@escape_chars (if (eq '__undefined__ def ) "" (@pretty_print def ))) )
                    (setq doc  (@escape_chars (if (@nonblankstring? doc) (lsprintf " ; %s" doc) "")) )
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
      (when (memq '@out (get name '?)) (fprintf port " => %s" (@pretty_print (get name '@out))))
      (getOutstring port)
      ));with ;fun

  (@fun clean_at_sign
    ( ( str ?type string )
      )
    ?doc "Remove backslash before 'at sign' when found at the beginning of names in STR."
    (pcreReplace (pcreCompile "\\B\\\\@") str "@" 0)
    )

  (@test
    ?fun 'title

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
           (fprintf port "%s" (@escape_chars doc))
         (fprintf port "%s" (@escape_chars (@str "Missing documentation for function `{(title name)}'.")))
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
            (fprintf port "%s\n"    (@escape_chars (clean_at_sign (@pretty_print input  ))))
            (foreach (str prefix color) (list info warn error)
                                        '( "INFO" "WARNING" "ERROR" )
                                        '( "darkblue" "darkorange" "darkred" )
              (when str
                (foreach line (parseString str "\n")
                  (fprintf port "<font color='%s'>;%s>%s</font>\n" color prefix (@escape_chars (clean_at_sign line)))
                  ));when ;foreach
              );foreach
            (fprintf port ";> %s\n" (@escape_chars (clean_at_sign (@pretty_print output ))))
            (when (cdr assertions) (newline port))
            ));let ;foreach
        (fprintf port "</pre>")
        );when
      (getOutstring port)
      ));with ;fun

  (@fun @docgen
    ( @key ( files ?type ( string ... )                                                            )
           ( init  ?type string         ?def (or (getShellEnvVar "SKILL_SHARP_INIT_COMMAND"  ) "") )
           ( before ?type string        ?def (or (getShellEnvVar "SKILL_SHARP_BEFORE_COMMAND") "") )

      @rest _
      )
    ?doc   "Load all SKILL or SKILL++ FILES.
Print associated documentation (as .fnd file content) to stdout."
    ?out    t|nil
    ?global t
    (destructuringBind ( functions _variables _scheme _classes _symbols )
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
        (@fprintf (@poport) "\
( \"{(title    function)}\"\n\
  \"{(syntax   function)}\"\n\
  \"{(abstract function)}\"\n\
  )\n"
          ));@fprintf ;foreach function

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
        ( (cdddr sexp)
          (warn "List contains more than three elements: %N" sexp)
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
      (@with ( ( port (instring (@exact_replace "\\@" (@file_contents file) "\\\\@")) )
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

