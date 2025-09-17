;; ===============================================================================================================
;; Fully custom Lint as native one was too buggy and not suitable for SKILL++ and SKILL#.
;; 
;; A. Buchet - September 2025
;; ===============================================================================================================

;; DEBUG
;(@set_debug t)

;; =======================================================
;; Add Rule
;; =======================================================

(@fun @lint_default_rule
  ( ( sexp     ?type list  )
    ( messages ?type tconc )
    ( levels   ?type list  )
    ( parents  ?type list  )
    )
  ?doc "Apply Lint recursively on all sublists of SEXP.
This is the default Lint 'control' rule."
  ?out nil
  ;(@debug "@lint_default_rule - sexp: {sexp}\n  messages: {messages}\n\n")
  (let ( ( sexp_pos 0)
         )
    (foreach sub_sexp sexp
      (_\@lint_sexp sub_sexp messages (cons sexp_pos++ levels) (cons sexp parents))
      ))
  nil)

(let ( ( control_rule_by_fun (makeTable t '@lint_default_rule) )
       ( rules_by_fun        (makeTable t nil) )
       )

  (@fun @lint_rule
    ( @key
      ( functions ?type ( symbol ... )          )
      ( control   ?type t|nil          ?def nil )
      ( rule_fun  ?type funobj                  )
      )
    ?doc "Define a Lint rule for FUNCTIONS."
    ?out t
    ?global t
    (foreach function functions
      (if control
          (setf control_rule_by_fun[function] rule_fun)
        (pushf rule_fun rules_by_fun[function])
        ))
    t
    );fun

  (@fun _\@lint_get_rules
    ( ( fun ?type symbol ) )
    ?doc "Return rules associated to FUN."
    ?out ( callable ... )
    (cons control_rule_by_fun[fun] rules_by_fun[fun])
    )

  (defglobalfun _\@lint_sexp ( sexp messages levels parents )
    "Lint SEXP."
    ;(@debug "_@lint_sexp\n  sexp: {sexp}\n  messages: {messages}\n\n")
    (cond
      ;; Symbol, check variable
      ( (symbolp sexp)
        ;; TODO - Make sure variable exists, mark it as used
        
        )
      ;; Atom, skip it
      ( (atom sexp)
        nil
        )
      ;; Non-nil list, parse it
      (t
        (destructuringBind (fun @rest body) sexp
          (cond
            ;; Apply available rules
            ( (symbolp fun)
              (foreach rule (_\@lint_get_rules fun)
                (funcall rule sexp messages levels parents)
                ))
            ;; TODO - Lists might be callable
            
            ;; Any other arugment is not supposed to be called
            ( t (@lint_msg sexp messages levels 'WARNING 'NOT_CALLABLE (@str "Not callable")) )
            ));cond ;dbind
        );t
      ));cond ;fun

  );closure


(defun @lint_msg ( sexp messages levels type name msg )
  (tconc messages (list type name levels msg sexp))
  )

;; -------------------------------------------------------
;; Lint waiver
;; -------------------------------------------------------

(@macro @no_lint ( @rest body )
  "Lint waiver, equivalent to `progn'."
  (constar 'progn "NO_LINT" body))

(@lint_rule
  ?functions '( @no_lint )
  ?control t
  ?rule_fun '@nil
  )

;; -------------------------------------------------------
;; progn
;; -------------------------------------------------------

(@lint_rule
  ?functions '( progn )
  ?control t
  ?rule_fun
  (lambda ( sexp @rest args )
    (unless (equal "NO_LINT" (nth 1 sexp)) (apply '@lint_default_rule sexp args))
    ))

;; -------------------------------------------------------
;; if, when, unless
;; -------------------------------------------------------

(@lint_rule
  ?functions '( if )
  ?rule_fun
  (lambda ( sexp messages levels @rest _ )
    ;; Extra arguments
    (when (nthcdr 4 sexp)
      (@lint_msg sexp messages levels 'ERROR 'IF_EXTRA_ARGS (@str "`if` only takes three arguments, those are extra: {(nthcdr 4 sexp)}"))
      )
    ;; (if <cond> nil ...) can be replaced by `unless`
    (unless (nth 2 sexp)
      (@lint_msg sexp messages levels 'INFO 'IF_NIL (@str "(if <cond> nil ...) can be replaced by (unless <cond> ...)"))
      )
    ))

(@lint_rule
  ?functions '( if when unless )
  ?rule_fun
  (lambda ( sexp messages levels @rest _ )
    ;; Static condition
    (let ( ( condition (nth 1 sexp) )
           )
      (when (or (eq t   condition)
                (eq nil condition)
                (not (memq (type condition) '( list symbol )))
                (and (listp condition) (eq 'quote (car condition)))
                )
        (@lint_msg sexp messages levels 'WARNING 'STATIC_CONDITION (@str "`{(car sexp)}` is useless, condition is static: {condition}"))
        ))
    ))

;; -------------------------------------------------------
;; (car (setof ...))
;; -------------------------------------------------------

(@lint_rule
  ?functions '( setof )
  ?rule_fun
  (lambda ( sexp messages levels parents @rest _ )
    (when (eq 'car (caar parents))
      (@lint_msg sexp messages (cdr levels) 'INFO 'CAR_SETOF "(car (setof ...)) can almost always be replaced by (car (exists ...))")
      ))
  )

;; -------------------------------------------------------
;; case-like functions
;; -------------------------------------------------------

(@lint_rule
  ?functions '( case caseq @case @caseq )
  ?control t
  ?rule_fun
  (lambda ( sexp messages levels parents )
    (cond
      ;; Check minumun number of arguments
      ( (not (cddr sexp)) (@lint_msg sexp messages levels 'ERROR 'CASE_MISSING_ARGS (@str "{(car sexp)} requires at least two arguments.")) )
      ( t
        (destructuringBind ( fun _val @rest cases ) sexp
          (let ( ( case_sexp_pos 2 )
                 )
            ;; Browse cases
            (foreach case cases
              (cond
                ( (not (listp case))
                  (@lint_msg sexp messages levels 'ERROR 'CASE_SYNTAX (@str "{fun} argument should be a list: {case}"))
                  )
                ;; Parse result S-expressions
                ( t
                  (let ( ( sexp_pos 1 )
                         )
                    (foreach sub_sexp (cdr case)
                      (_\@lint_sexp sub_sexp messages (constar sexp_pos++ case_sexp_pos++ levels) (constar case sexp parents))
                      ));foreach ;let
                  ) ;t
                ));cond ;foreach case
            );let
          ));dbind ;t
      ));cond ;lambda
  )

;; =======================================================
;; Apply rules
;; =======================================================

(let ()

  (@fun @lint
    ( @key
      ( files ?type ( string ... )                )
      ( port  ?type port           ?def (@poport) )
      )
    ?doc "Run Sharper Lint on FILES.
All report messages are printed to PORT."
    ?out t|nil
    ?global t
    ;; -------------------------------------------------------
    ;; Parse S-expressions of each file
    ;; -------------------------------------------------------
    (let ( ( results_by_file (tconc nil nil) )
           ( lint_status     t               )
           )
      (@letf ( ( (status optimizeTailCall) t )
               ( @str.pretty               t )
               )
        (foreach file files
          (let ( ( in_port             (infile file)   )
                 ( pos                 0               )
                 ( line                1               )
                 ( results_by_top_sexp (tconc nil nil) )
                 )
            (@while (lineread in_port)
              ?var sexps
              (let ( ( messages (tconc nil nil) )
                     )
                (if (eq t sexps)
                    ;; EOF not reached yet but line does not contain Lisp code
                    (progn line++ (setq pos (fileTell in_port)))
                  ;; Parse S-expression
                  (let ( ( beg_pos  pos                                                                      )
                         ( beg_line line                                                                     )
                         ( end_line (setq line line+(count_lines in_port pos (setq pos (fileTell in_port)))) )
                         ( sexp_pos 0                                                                        )
                         )
                    (foreach sexp sexps
                      (_\@lint_sexp sexp messages (list sexp_pos++) sexps)
                      );foreach
                    ;; Find actual lines where messages occured
                    (setq messages
                      (foreach mapcar message (cdar messages)
                        (setf (nth 2 message) (find_line in_port beg_pos pos beg_line (reverse (nth 2 message))))
                        message
                        ))
                    ;; Store result by top S-expressions
                    (tconc results_by_top_sexp (list beg_pos beg_line:end_line messages))
                    ));let ;if
                ));let ;while
            ;; Store result by file
            (tconc results_by_file (list file (cdar results_by_top_sexp)))
            ));let ;foreach
        ;; -------------------------------------------------------
        ;; Format messages in a nice report
        ;; -------------------------------------------------------
        (@fprintf port "Running Lint - {(getCurrentTime)}\n")
        (@foreach_dbind ( file res )  (cdar results_by_file)
          (@fprintf port "File {file}:\n")
          (@foreach_dbind ( beg_pos ( beg_line end_line ) messages ) res
            ;; No need to print where top-level S-expressions are found
            ;; Only the info, warnings and errors matter
            ; (when messages
            ;   (if (eq beg_line (sub1 end_line))
            ;       (@fprintf port "  Top-level S-Expression at line {beg_line}:\n")
            ;     (@fprintf port "  Top-level S-Expression at lines {beg_line} - {(sub1 end_line)}:\n")
            ;     ))
            (@foreach_dbind ( type name line text sexp ) messages
              (@fprintf port "  {type%7s} {name} at line {line%-3d} - {text} - {sexp}\n")
              (@caseq type
                ( ( ERROR WARNING ) (setq lint_status nil) )
                ( ( INFO          ) ()                     )
                ));caseq;foreach_dbind message
            );foreach_dbind lines
          (newline port)
          );foreach_dbind file
        (println (if lint_status 'PASS 'FAIL))
        ;; Return status
        lint_status
        );letf
      ));let ;fun

  (defun count_lines ( port beg_pos end_pos )
    "Count the newlines between BEG_POS and END_POS in PORT."
    (fileSeek port beg_pos 0)
    (let ( ( lines 0 )
           )
      (for i beg_pos end_pos-1
        (when (eq '\n (getc port)) lines++)
        )
      lines
      ));let ;fun

  (defun find_line ( port beg_pos end_pos beg_line levels )
    "Browse LEVELS in PORT starting from BEG_POS to find actual line number."
    (@wrap (fileSeek port beg_pos 0)
           (fileSeek port end_pos 0)
      (let ( ( lines beg_line )
             )

        (@debug "\n\nfind_line: beg_pos {beg_pos}, beg_line {beg_line}, levels {levels}")

        (defun ignore_whitespace ()
          "Move port point until a non-whitespace character is found."

          (@debug "ignore_whitespace: {(fileTell port)}, lines {lines}")

          ;; Commas are considered as whitespace
          (prog ( char
                  )
            (while t
              (setq char (getc port))
              (cond
                ( (memq char '( \  \t \, )) nil)
                ( (eq char '\n) lines++ )
                ;; Also ignore line and block comments
                ( (eq char '\;) (ignore_line_comment) )
                ( (and (eq char '\/) (or (eq (getc port) '\*) (and (fileSeek port -1 1) nil)))
                  (ignore_block_comment)
                  )
                ( t            (fileSeek port -1 1) (return) )
                ))
            ));prog ;fun

        (defun next_sexp ()
          "Browse next characters until a valid S-exp has been found."

          (@debug "next_sexp: {(fileTell port)}, lines {lines}")

          (prog ( char
                  )
            (while t
              (setq char (getc port))
              (cond
                ;; Whitespace, S-exp is finished
                ( (memq char '( \  \, \t )) (return) )
                ( (eq char '\n) lines++ (return) )
                ;; Backslash, ignore next character
                ( (eq char '\\) (when (eq '\n (getc port)) lines++) )
                ;; Line comment, ignore it
                ( (eq char '\;) (ignore_line_comment) )
                ;; Block comment, ignore it
                ( (and (eq char '\/) (or (eq (getc port) '\*) (and (fileSeek port -1 1) nil)))
                  (ignore_block_comment)
                  )
                ;; String, ignore it and return
                ( (eq char '\") (ignore_string) (return) )
                ;; Bracketted-expression, ignore it and return
                ( (memq char '( \( \{)) (ignore_bracket char) (return) )
                ;; EOF
                ( (not char) (return) )
                ;; TODO - Handle special characters [ + - * / % : && || ]
                
                ));cond ;while
            ));prog ;fun

        (defun ignore_line_comment ()
          "Move port point until end-of-line is reached."

          (@debug "BEGIN - ignore_line_comment: {(fileTell port)}, lines {lines}")

          (prog ()
            (while t
              (caseq (getc port)
                ( ( \\ ) (when (eq '\n (getc port)) lines++) )
                ( ( \n ) lines++ (return)                    )
                ( nil    (return)                            )
                ));case ;while
            )
          (@debug "END - ignore_line_comment: {(fileTell port)}, lines {lines}")
          );prog ;fun

        (defun ignore_block_comment ()
          "Move port point until end of block comment is reached."

          (@debug "ignore_block_comment: {(fileTell port)}, lines {lines}")

          (prog ()
            (while t
              (caseq (getc port)
                ( ( \n ) lines++                                             )
                ( ( \* ) (caseq (getc port) ( \n lines++ ) ( \/ (return) ) ) )
                ( nil    (return)                                            )
               ));case ;while
            ));prog ;fun

        (defun ignore_string ()
          "Move port point until next double-quote is reached."

          (@debug "ignore_sring: {(fileTell port)}, lines {lines}")

          (prog ()
            (while t
              (caseq (getc port)
                ( ( \\ ) (when (eq '\n (getc port)) lines++) )
                ( ( \n ) lines++                             )
                ( ( \" ) (return)                            )
                ( nil    (return)                            )
              ));case ;while
            ));prog ;fun

        (defun ignore_bracket ( @optional (open_char '\() )
          "Move port point until matching bracket is reached."

          (@debug "ignore_bracket {open_char}: {(fileTell port)}, lines {lines}")

          (prog ( ( close_char (@caseq open_char ( \( '\) ) ( \{ '\} )) )
                  ( n          1                                        )
                  char
                  )
            (while t
              (setq char (getc port))
              (cond
                ( (eq char '\\) (when (eq '\n (getc port)) lines++) )
                ( (eq char '\n) lines++ )
                ( (eq char '\;) (ignore_line_comment) )
                ( (and (eq char '\/) (or (eq (getc port) '\*) (and (fileSeek port -1 1) nil)))
                  (ignore_block_comment)
                  )
                ( (eq char '\") (ignore_string) )
                ( (eq char open_char ) n++ )
                ( (eq char close_char) n-- (when (zerop n) (return)) )
                ( (not char) (return) )
               ));cond ;while
            ));prog ;fun

        (defun next_bracket ()
          "Move port point until an open bracket is reached."

          (@debug "next_bracket: {(fileTell port)}, lines {lines}")

          (prog ( char
                  c_style
                  )
            (while t
              (setq char (getc port))
              (cond
                ( (eq char '\\) (when (eq '\n (getc port)) lines++) )
                ( (eq char '\n) lines++ )
                ( (eq char '\;) (ignore_line_comment) )
                ( (and (eq char '\/) (or (eq (getc port) '\*) (and (fileSeek port -1 1) nil)))
                  (ignore_block_comment)
                  )
                ( (eq char '\") (ignore_string))
                ( (memq char '( \( \{ ) ) (return c_style) )
                ( (not char) (return c_style) )
                ;; If a different character is encountered here it probably means the current syntax is C-style
                ( t (setq c_style t) )
                ));cond ;while
            ));prog ;fun

        ;; Browse each nested S-expression level
        (while levels
          ;; Move to reach argument position
          (let ( ( arg_pos (pop levels) )
                 )
            ;; Advance point in port until position is reached
            (while (plusp arg_pos)
              (ignore_whitespace)
              (next_sexp)
              arg_pos--
              )
            ;; Going-down another level, find next open-bracket
            (ignore_whitespace)
            (when levels
              (when (next_bracket)
                ;; C-style, reduce arguments count for next level
                (setf (car levels) (sub1 (car levels)))
                ))
            ));let ;while
        (@debug "-> final_pos {(fileTell port)}, lines {lines}")
        lines
        );let
      ));wrap ;fun

  );closure

;*/

