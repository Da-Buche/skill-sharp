;; ===============================================================================================================
;; Python-like f-strings macro
;;
;; A. Buchet - July 2025
;; ===============================================================================================================

(@fun @to_string
  ( ( obj  ?type any                 ?doc "Any S-expression to be returned as a string"                   )
    ( spec ?type string|nil ?def nil ?doc "Format specification as described in `fprintf' documentation." )
    )
  ?doc "Return OBJ as a string (like `printself').
If OBJ is already a string, it is returned as is.
If @str.pretty is non-nil and OBJ is a list: it is pretty-printed.

This function is meant to be used as `@str' macro helper to avoid double-quotes when inserting strings.
Those double-quotes are easily added by hand when required while removing afterwards is way more complicated.
This design choice is also consistent with Python's f-strings."
  ?out string
  (cond
    ( spec                           (lsprintf spec obj)           )
    ( (stringp obj)                  obj                           )
    ;; Fix backslashes when printing symbols
    ( (symbolp obj)                  (strcat obj)                  )
    ( (and (listp obj) @str.pretty ) (@pretty_print obj)           )
    ( t                              (lsprintf (or spec "%N") obj) )
    ));cond ;fun

(let ( in out args char translate error_message )

  (@fun translate_as_is ()
    ?doc "Write characters as is from input port until an open bracket is found."
    ?out t
    (while (and (setq char (getc in))
                (neq char '\{)
                )
      (fprintf out "%s" char)
      );while
    ;; Toggle translating function
    (setq translate translate_evaluated)
    ;; Return t
    t
    );def

  (@fun translate_evaluated ()
    ?doc "Read characters from input port until closing bracket is found.
A %s is printed to output port, while read string is added to S-expressions to evaluate."
    ?out t
    (setq char (getc in))
    (cond
      ;; End-of-port is reached,
      ;; generic error is raised outside `cond'
      ( (not char)
        nil
        )
      ;; First evaluated character is a second bracket,
      ;; print one open bracket without evaluation
      ( (eq '\{  char)
        (fprintf out "{")
        )
      ;; First evaluated character is a closing bracket,
      ;; raise an error
      ( (eq '\} char)
        (setq error_message "Empty {}")
        )
      ;; Any other character to translate,
      ;; read the whole block
      ( t
        (let ( ( format_spec nil )
               sexp_str
               sexp
               )
          ;; Read and store the whole block
          (@with ( ( port (outstring) )
                     )
            (fprintf port "%c" char)
            (while (and (setq char (getc in))
                        (neq char '\})
                        )
              (fprintf port "%c" char)
              );while
            (setq sexp_str (getOutstring port))
            );with
          ;; Try to deduce format specification
          ;; Percentage symbol is doubled in the beginning of `_\@str'
          (when (pcreMatchp "(.*)%(%-?[0-9]*(\\.?[0-9]+)?[doxfegscnPBNLA])$" sexp_str)
            (setq sexp_str    (pcreSubstitute "\\1"))
            (setq format_spec (pcreSubstitute "\\2"))
            )
          ;; Try to read f-string block content as SKILL code
          (assert (setq sexp (errset (car (linereadstring (lsprintf "{%s}" sexp_str)))))
            "Error while parsing f-string : %N\n%N" sexp_str errset.errset)
          ;; Print placeholder in translated string and add parsed expressions to `lsprintf' arguments
          (fprintf out "%s" "%s")
          (setq sexp (car sexp))
          ;; Remove `progn' when it contains only one sexp
          ;; This is just to avoid waiving associated Lint rule
          (when (and (eq 'progn (car sexp))
                     (cdr sexp)
                     (not (cddr sexp))
                     )
            (setq sexp (cadr sexp))
            )
          (tconc args (list '@to_string sexp format_spec))
          );let
        )
      );cond
    (unless char (setq error_message "Open-bracket is never closed"))
    ;; Toggle translating function
    (setq translate translate_as_is)
    ;; Return t
    t
    )

  (@fun _\@str
    ( ( str ?type string ?doc "Python f-string to be translated." )
      )
    ?doc    "`@str' helper, parse STR as a Python f-string and return equivalent `lsprintf' arguments."
    ?out    ( string list ... )
    ?global t
    ;; Support "%" and "}}" in input string
    (setq str (pcreReplace (pcreCompile "%" ) str "%%" 0))
    (setq str (pcreReplace (pcreCompile "}}") str "}"  0))
    ;; Translate string using string-ports
    (@with ( ( in_port  (instring str) )
             ( out_port (outstring   ) )
             )
      (setq in  in_port )
      (setq out out_port)
      ;; Initialize closure variables
      (setq args          (tconc nil nil))
      (setq translate     translate_as_is)
      (setq char          t              )
      (setq error_message nil            )
      ;; Parse string
      (while char (translate))
      (when error_message (error "%s in f-string : %N" error_message str))
      ;; Return translated string and associated S-expressions to be evaluated
      (list (getOutstring out_port) (prog1 (cdar args) (setq args (tconc nil nil))))
      ))

  );closure

(@macro @str ( str "t" )
  "Format STR using Python f-strings rules.
Everything between '{' and '}' will be evaluated in current environment.
'{{' and '}}' can be used to insert '{' and '}' in the formatted string.

Expressions inside '{' and '}' can end with any format specification like %A or %-12.27f.
(As described in `fprintf' documentation.)

If @str.pretty is non-nil, lists are pretty-printed.
"
  ;; No idea why the type letter in arguments template is not taken in account
  ;; Without the following line, when running (@str symbol)
  ;; The returned error comes from _\@str body and is:
  ;; pcreReplace: argument #2 should be a string
  (assert (stringp str) "@str: argument #1 should be a string (type template = \"t\") - %N" str)
  (destructuringBind ( format_str args ) (_\@str str)
    (constar 'lsprintf format_str args)
    ))

;; -------------------------------------------------------
;; f-string macros
;; -------------------------------------------------------

(@macro @debug ( message )
  "Print debugging MESSAGE using f-string format.
This is `_\\@debug' wrapper using `@str' macro."
  `(funcall '_\@debug "%s" (@one_newline (@str ,message)))
  )

(@macro @info ( message )
  "Print info MESSAGE using f-string format.
This is `info' wrapper using `@str' macro."
  `(info "%s" (@one_newline (@str ,message)))
   )

(@macro @warn ( message )
  "Print warning MESSAGE using f-string format.
This is `warn' wrapper using `@str' macro."
  `(warn "%s" (@one_newline (@str ,message)))
   )

(@macro @error ( message )
  "Print error MESSAGE using f-string format.
This is `error' wrapper using `@str' macro."
  `(error "%s" (@one_newline (@str ,message)))
   )

(@macro @assert ( test message )
  "Write assert using f-string format.
This is `assert' wrapper using `@str' macro."
  `(assert ,test "%s" (@one_newline (@str ,message)))
   )

(@macro @fprintf ( port message )
  "Print MESSAGE to PORT.
This is `fprintf' wrapper using `@str' macro."
  `(fprintf ,port "%s" (@str ,message))
   )

;; TODO - @yesno

;*/

