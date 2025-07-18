;; ===============================================================================================================
;; Lint rules and associated functions
;;
;; A. Buchet - April 2025
;; ===============================================================================================================

;; =======================================================
;; Custom macros to define rules
;; =======================================================

(@macro _\@lint_add_rule ( names test @rest body )
  "`SK_RULE' wrapper to make custom rules waivable all together using Shell variable 'SKILL_SHARP_NO_LINT_RULES'."
  `(SK_RULE ,names (unless (equal "TRUE" (getShellEnvVar "SKILL_SHARP_NO_LINT_RULES")) ,test)
     ;; No idea why, this variable is defined but not used in rules BODY
     ;; this is a dirty waiver...
     (when (boundp 'formlist) formlist)
     ,@body))

(@macro _\@lint_add_control_rule ( names test @rest body )
  "`SK_RULE SK_CONTROL' wrapper to make custom rules waivable all together using Shell variable 'SKILL_SHARP_NO_LINT_RULES'."
  `(SK_RULE SK_CONTROL ,names (unless (equal "TRUE" (getShellEnvVar "SKILL_SHARP_NO_LINT_RULES")) ,test)
     (when (boundp 'formlist) formlist)
     ,@body))


;; =======================================================
;; Lint waiver
;; =======================================================

(@macro @no_lint ( @rest body )
  "Lint waiver, equivalent to `progn'."
  (constar 'progn "NO_LINT" body))

;; Waive `@no_lint' expressions.
;; Also waive `progn' calls starting with "NO_LINT".

(_\@lint_add_control_rule ( @no_lint progn ) t
  ;; Check `progn' first argument
  (@caseq (SK_FUNCTION)
    ( @no_lint nil )
    ( progn
      (unless (equal "NO_LINT" (car (SK_ARGS)))
        (foreach map sexp (SK_ARGS) (SK_CHECK_FORM sexp))
        ))
    ))


;; ===============================================================================================================
;; Rules
;; ===============================================================================================================

;;     ^
;;    / \    Warning:
;;   / | \   `map' syntax does not work in `SK_RULE' calls.
;;  /  .  \  It is required to use `foreach map' instead.
;; /_______\

;; =======================================================
;; Native Code Rules
;; =======================================================

;; -------------------------------------------------------
;; Unknown `status' or `sstatus' calls
;; -------------------------------------------------------

;; This is required at least when running Lint from the SKILL Interpreter
(_\@lint_add_rule ( status sstatus ) (not (errset (funcall 'status (car (SK_ARGS)))))
  (SK_ERROR UNKNOWN_STATUS_FLAG "Unknown (s)status flag: %N\n" (SK_FORM)))


;; -------------------------------------------------------
;; (car (setof ...)) to replace by (car (exists ...))
;; -------------------------------------------------------

(_\@lint_add_rule ( setof ) (equal 'car (caar (SK_FORM 2)))
  (SK_HINT CAR_SETOF "(car (setof ...)) should be replaced by (car (exists ...)): %N" (SK_FORM))
  )


;; -------------------------------------------------------
;; Check missing docstrings
;; -------------------------------------------------------

(_\@lint_add_rule ( procedure globalProc ) (not (stringp (cadr (SK_ARGS))))
  (SK_WARNING MISSING_DOCSTRING "procedure %N has no docstring\n" (caar (SK_ARGS))))

(_\@lint_add_rule ( defun defglobalfun ) (not (stringp (caddr (SK_ARGS))))
  (SK_WARNING MISSING_DOCSTRING "function %N has no docstring\n" (car (SK_ARGS))))

(_\@lint_add_rule ( defmethod )
  (if (memq (cadr (SK_ARGS)) '( @before @after @around ))
      (not (stringp (cadddr (SK_ARGS))))
    (not (stringp (caddr (SK_ARGS))))
    )
  (SK_WARNING MISSING_DOCSTRING "Method %N has no docstring\n" (car (SK_ARGS))))


;; -------------------------------------------------------
;; Symbol as argument
;; -------------------------------------------------------

; (_\@lint_add_rule ( lambda ) (symbolp (car (SK_ARGS)))
;   (destructuringBind ( args_name @rest body ) (SK_ARGS)
;     (SK_PUSH_VAR args_name)
;     (foreach map sexp body (SK_CHECK_FORM sexp))
;     (SK_POP_VAR args_name)
;     ))

; (_\@lint_add_control_rule ( lambda ) t
;  (cond
;    ( (not (SK_ARGS))
;      (SK_ERROR LAMBDA "`lambda' requires arguments\n")
;      )
;    ;; Argument is a symbol
;    ( (symbolp (car (SK_ARGS)))
;      (destructuringBind ( args_name @rest body ) (SK_ARGS)
;        (SK_PUSH_VAR args_name)
;        (foreach map sexp body (SK_CHECK_FORM sexp))
;        (SK_POP_VAR args_name)
;        ))
;    ;; TODO - lambda Argument is a list
;    ( t nil )
;    ))


;; =======================================================
;; Custom Code Rules
;; =======================================================

;; -------------------------------------------------------
;; For most macros, expand them and check expanded form
;; -------------------------------------------------------

(_\@lint_add_control_rule ( @macro @fun @class
                            @case @caseq
                            @str @debug @info @warn @error @assert @fprintf @yesno
                            )
  t
  ;; If error occurs inside rule, this should raise WARN MACROEXP1 by default
  (SK_CHECK_FORM (errset (expandMacro (SK_FORM))))
  )


;; -------------------------------------------------------
;; Python f-strings
;; -------------------------------------------------------

(_\@lint_add_rule ( @str ) (stringp (car (SK_ARGS)))
  (unless (index (car (SK_ARGS)) "{")
    (SK_HINT EXTRA_FSTRING "No need to use `@str' for non-formatted string: %N" (car (SK_ARGS)))
    ))

;; TODO - Check more advanced f-string cases (never opened closing-bracket, brackets inside evaluated part, ...)
; (_\@lint_add_rule (@str) (stringp (car (SK_ARGS)))
;   (let ( ( str (car (SK_ARGS)) )
;          )
;     ;; Remove double brackets
;     (setq str (pcreReplace (pcreCompile "({{|}})") str "" 0))

;   (foreach map sexp (expandMacro (SK_FORM))
;     (SK_CHECK_FORM sexp)
;     ))


;; -------------------------------------------------------
;; Anaphoric Macros
;; -------------------------------------------------------

(_\@lint_add_control_rule ( @if @nif @when ) t
  ;; Expand macro to raise WARN MACROEXP1 in case of errors
  (errset (expandMacro (SK_FORM)))
  ;; Check arguments
  (destructuringBind ( @key var @rest args ) (SK_ARGS)
    (@nif var
          ;; Var is not used, check expressions
          (foreach map sexp args (SK_CHECK_FORM sexp))
      ;; Var is used, check test, then check 'then' and 'else' statements.
      (SK_CHECK_FORM args)
      (SK_PUSH_VAR var)
      (foreach map sexp (cdr args) (SK_CHECK_FORM sexp))
      (SK_POP_VAR var)
      )))

(_\@lint_add_control_rule ( @wrap ) t
  ;; Expand macro to raise WARN MACROEXP1 in case of errors
  (errset (expandMacro (SK_FORM)))
  ;; Check arguments
  (destructuringBind ( in out @rest body ) (SK_ARGS)
    (unless (or in out)
      (SK_HINT EXTRA_WRAP "@wrap without IN or OUT can be removed or replaced by progn")
      )
    ;; Check in, out and body
    (SK_CHECK_FORM (list in ))
    (SK_CHECK_FORM (list out))
    (foreach map sexp body (SK_CHECK_FORM sexp))
    ))

(_\@lint_add_control_rule ( @letf ) t
  ;; Expand macro to raise WARN MACROEXP1 in case of errors
  (errset (expandMacro (SK_FORM)))
  ;; Check arguments
  (destructuringBind ( defs @rest body ) (SK_ARGS)
    (unless (and (listp defs) (forall def defs (and (listp def) (cdr def) (not (cddr def)))))
      (SK_ERROR SP_WITH_DEFS "`@letf' first argument should be a list of expression-value pairs: %N" defs)
      )
    (let ( ( vars (mapcar 'car defs) )
           )
      ;; Check definitions, then check body
      (foreach map def  defs
        (SK_CHECK_FORM (list (caar def)))
        (SK_CHECK_FORM (cdar def))
        )
      (foreach map sexp body (SK_CHECK_FORM sexp      ))
      )))

(_\@lint_add_control_rule ( @with ) t
  ;; Expand macro to raise WARN MACROEXP1 in case of errors
  (errset (expandMacro (SK_FORM)))
  ;; Check arguments
  (destructuringBind ( defs @rest body ) (SK_ARGS)
    (unless (and (listp defs) (forall def defs (and (listp def) (symbolp (car def)) (cdr def) (not (cddr def)))))
      (SK_ERROR SP_WITH_DEFS "`@with' first argument should be a let-like list of definitions: %N" defs)
      )
    (let ( ( vars (mapcar 'car defs) )
           )
      ;; Check definitions, then add variables, check body and remove variables
      (foreach map def  defs (SK_CHECK_FORM (cdar def)))
      (foreach map var  vars (SK_PUSH_VAR   (car  var)))
      (foreach map sexp body (SK_CHECK_FORM sexp      ))
      (foreach map var  vars (SK_POP_VAR    (car  var)))
      )))

;; -------------------------------------------------------
;; Testing
;; -------------------------------------------------------

(_\@lint_add_control_rule ( @test ) t
  (destructuringBind ( @key fun title doc skip @rest body ) (SK_ARGS)
    (foreach map sexp (list fun title doc skip) (SK_CHECK_FORM (list (car sexp))))
    (foreach map sexp body                      (SK_CHECK_FORM sexp             ))
    ))

(_\@lint_add_control_rule ( @assertion ) t
  (destructuringBind ( @key doc skip info warn error out @rest body ) (SK_ARGS)
    (foreach map sexp (list skip info warn error out) (SK_CHECK_FORM (list (car sexp))))
    (foreach map sexp body                            (SK_CHECK_FORM sexp             ))
    ));dbind



;; ===============================================================================================================
;; Sharp Lint Functions
;; ===============================================================================================================

(let ( ignores )

  (@fun add_ignore
    ( ( name ?type symbol )
      ( doc  ?type string )
      )
    ?doc "Ignore rule named NAME using DOC as explanation.
(This function is only meant to force developers to provide a justification when waiving rules.)"
    (push (list name doc) ignores)
    )

  (add_ignore 'REP110   "No one cares about number of total definitions, only the score matters!"          )

  (add_ignore 'VAR13    "Variable starting with underscore are ignored on purpose!"                        )
  (add_ignore 'DEFMET4  "Variable starting with underscore are ignored on purpose!"                        )

  ;; TODO - Rewrite rule to check empty `let'
  (add_ignore 'LET2     "This rule does not support custom macros like `@fun'"                             )

  ;; TODO - Rewrite rule to check `defun' arguments
  (add_ignore 'CHK15    "Report errors when funtion arguments is a symbol while this is a valid syntax..." )

  (add_ignore 'STATUS2  "Replaced by custom rules as it raises errors for valid statuses."                 )

  (@fun _\@lint_get_ignores ()
    ?doc    "Return `sklint' ignored rules names."
    ?out    ( symbol ... )
    ?global t
    (mapcar 'car ignores)
    )

  );closure

;; Defining the following function in SKILL to avoid the following warning:
;; *WARNING* (sklint): calling NLambda from Scheme code

(inSkill

  (@fun @lint
    ( @key
      ( files        ?type ( string ... )                                                                      )
      ( ignores      ?type list           ?def ()                                                              )
      ( file_by_file ?type t|nil          ?def (equal "TRUE" (getShellEnvVar "SKILL_SHARP_LINT_FILE_BY_FILE")) )
      )
    ?doc    "`sklint' wrapper with custom ignores."
    ?out    nil
    ?global t
    (@letf ( ( (status printinfix) nil )
             )
      (if file_by_file
          (forall file files
            (sklint ?file file ?ignores (nconc ignores (_\@lint_get_ignores))))
        (sklint ?file files ?ignores (nconc ignores (_\@lint_get_ignores)))
        );if
      ));letf ;fun

  );inSkill

;; TODO - Use (car (exists ...)) instead of (car (setof ...)) rule.
;; TOOD - Always use `printf, `fprintf', `lsprintf', ... with more than one argument, at least (fprintf "%s" str)

;     ))
;*/
