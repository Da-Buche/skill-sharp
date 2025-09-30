;; ===============================================================================================================
;; Fully custom Lint as native one was too buggy and not suitable for SKILL++ and SKILL#.
;;
;; A. Buchet - September 2025
;; ===============================================================================================================

;; =======================================================
;; Add Rule
;; =======================================================

;; -------------------------------------------------------
;; Default rule
;; -------------------------------------------------------

(@fun @lint_default_rule
  ( ( sexp     ?type list    )
    ( messages ?type tconc   )
    ( levels   ?type list    )
    ( parents  ?type list    )
    ( envs     ?type list    )
    ( scheme   ?type boolean )
    )
  ?doc "Apply Lint recursively on all sublists of SEXP.
This is the default Lint 'control' rule."
  ?out nil
  ;(@debug "@lint_default_rule - sexp: {sexp}\n  messages: {messages}\n\n")
  (let ( ( sexp_pos 1          )
         ( fun      (car sexp) )
         ( args     (cdr sexp) )
         )
    ;; Check function
    (caseq (type fun)
      ( symbol
        (cond
          ;; Variable is defined
          ( (and scheme (exists env envs (and (tablep env) env[fun])))
            ;; TODO - Make sure that variable describes a function

            )
          ;; Variable is callable
          ( (isCallable fun) )
          ;; Not callable...
          ( t (@lint_msg sexp messages levels 'WARNING 'UNCALLABLE "{fun} is not callable") )
          ))
      ( list
        ;; TODO - List might sometimes be callable

        )
      ( t (@lint_msg sexp messages levels 'WARNING 'UNCALLABLE "{fun} is not callable") )
      )
    ;; Check arguments
    (foreach sub_sexp args
      (@lint_sexp sub_sexp messages (cons sexp_pos++ levels) (cons sexp parents) envs scheme)
      ))
  nil)

;; -------------------------------------------------------
;; Functions to manage rules
;; -------------------------------------------------------

(let ( ( control_rule_by_fun (makeTable t nil) )
       ( rules_by_fun        (makeTable t nil) )
       )

  (@fun @lint_rule
    ( @key
      ( functions ?type ( symbol ... )          )
      ( control   ?type t|nil          ?def nil )
      ( rule_fun  ?type callable                )
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

  (defglobalfun @lint_sexp ( sexp messages levels parents envs scheme )
    "Lint SEXP."
    ;(@debug "_@lint_sexp\n  sexp: {sexp}\n  messages: {messages}\n\n")
    ;; TODO - At least when debugging, assert that levels and parents are matching (in terms of nested expressions)

    (cond
      ;; Symbol, check variable
      ( (symbolp sexp)
        ;; Make sure variable exists, mark it as used
        (@if (car (exists env envs env[sexp]))
             ?var env
             (setf env[sexp]->status 'used)
          (cond
            ;; Symbol is t, nothing to say
            ( (eq t sexp) )
            ;; Symbol is a key argument specifier
            ( (eq '? (getchar sexp 1)) )
            ;; Variable is bound
            ( (if scheme (boundp sexp (schemeTopLevelEnv)) (boundp sexp))
              (@lint_msg (car parents) messages levels 'WARNING 'GLOBAL_USE (@str "Global variable is used: {sexp}")
                ?predicate
                (lambda _
                  ;; Make sure function is not defined later
                  (not
                    (exists env envs
                      (let ( ( dpl (and (tablep env) env[sexp]) )
                             )
                        (when (eq 'function dpl->type) (setf dpl->status 'used) t)
                        )))
                  ))
              )
            ( t (@lint_msg (car parents) messages levels 'ERROR 'GLOBAL_USE (@str "Undefined global variable is used: {sexp}")
                  ?predicate
                  (lambda _
                    ;; Make sure function is not defined later
                    (not
                      (exists env envs
                        (let ( ( dpl (and (tablep env) env[sexp]) )
                               )
                          (when (eq 'function dpl->type) (setf dpl->status 'used) t)
                          )))
                    )) )
            );cond
          ))
      ;; Atom, skip it
      ( (atom sexp)
        nil
        )
      ;; Non-nil list, parse it
      (t
        (let ( ( fun (car sexp) )
               )
          (cond
            ;; Apply available rules
            ( (symbolp fun)
              ;; Find first environment where variable is used
              (@when (car (exists env envs (and (tablep env) env[fun])))
                ?var env
                (setf env[fun]->status 'called)
                )
              ;; TODO - If variable is in an environment, make sure it is callable and check its arguments if they are defined

              ;; Function is global, check its arguments and its rules
              (@nif (getd fun)
                    (progn
                      (@lint_msg sexp messages levels 'WARNING 'NOT_CALLABLE (@str "Function {fun} is not callable")
                        ;; Following predicate is here to guarantee that function is not defined afterwards in another environment
                        ?predicate
                        (lambda _
                          ;; Make sure function is not defined later
                          (not
                            (exists env envs
                              (let ( ( dpl (and (tablep env) env[fun]) )
                                     )
                                (when (eq 'function dpl->type) (setf dpl->status 'used) t)
                                )))
                          ))
                      ;; Check other arguments in case function is callable (if it's defined later for instance)
                      (let ( ( sexp_pos 1 )
                             )
                        (foreach sub_sexp (cdr sexp)
                          (@lint_sexp sub_sexp messages (cons sexp_pos++ levels) (cons sexp parents) envs scheme)
                          ))
                      )
                ;; Check function arguments
                (unless (errset (check_arguments sexp messages levels (get_arguments fun)) nil)
                  (@lint_msg sexp messages levels 'WARNING 'ARGS_CHECK_FAIL (@str "Error when checking arguments: {errset.errset}"))
                  )
                ;; Fetch and apply rules
                (prog ( ( control_rule control_rule_by_fun[fun] )
                        ( rules        rules_by_fun[fun]        )
                        expanded_sexp
                        )
                  ;; When a macro is encountered, try to expand it and report expansion errors
                  (@nif (isMacro fun)
                        ;; Not a macro, use default control rule unless provided
                        (push (or control_rule '@lint_default_rule) rules)
                    ;; Macro
                    (setq expanded_sexp (expand_macro sexp messages levels))
                    (unless expanded_sexp (return))
                    ;; If macro has no control rule, parse its expanded value
                    (if control_rule
                        (push control_rule rules)
                      (@lint_sexp expanded_sexp messages (cons 'STOP levels) (cons sexp parents) envs scheme)
                      ))
                  ;; Apply rules
                  (foreach rule rules
                    (unless (errset (funcall rule sexp messages levels parents envs scheme) nil)
                      (@lint_msg sexp messages levels 'WARNING 'LINT_ERROR (@str "Error when applying `{fun}` rule: {errset.errset}"))
                      ))
                  );prog
                ));nif ;symbolp
            ;; TODO - Lists might be callable

            ;; Any other arugment is not supposed to be called
            ( t (@lint_msg sexp messages levels 'WARNING 'NOT_CALLABLE (@str "Not callable")) )
            ));cond ;dbind
        );t
      ));cond ;fun

  (@fun check_arguments
    ( ( sexp      ?type list                    )
      ( messages  ?type list                    )
      ( levels    ?type list                    )
      ( arguments ?type ( list table list )|nil )
      )
    ?doc "Check if SEXP respects ARGLIST."
    (@nif arguments
      (@lint_msg sexp messages levels 'INFO 'UNSUPPORTED (@str "`{(car sexp)}` arguments check is not supported, see (@arglist `{(car sexp)}`)"))
      (destructuringBind ( positional_args key_args rest_arg ) arguments
        (let ( ( provided_args      (copy (cdr sexp)) )
               ( remaining_args     nil               )
               ( missing_args_count 0                 )
               )
          ;; Check positional arguments
          (foreach positional_arg positional_args
            (cond
              ;; Some provided arguments remain, pop one
              ( provided_args
                (let ( ( arg (pop provided_args) )
                       )
                  (and
                    (symbolp arg)
                    (eq '? (getchar arg 1))
                    (neq '? arg)
                    (neq 'quote (car sexp))
                    (@lint_msg sexp messages levels 'WARNING 'POSITIONAL_KEY_ARG (@str "`{(car sexp)}` argument {arg} is treated as positional, move or quote it for disambiguation"))
                    )
                  ))
              ;; Missing argument, check if argument is required or optional
              ( (memq '?def positional_arg->?) )
              ;; Missing positional argument, report it
              ( t missing_args_count++ )
              ));cond ;foreach
          (when (plusp missing_args_count)
            (@lint_msg sexp messages levels 'ERROR 'MISSING_ARG (@str "`{(car sexp)}` requires {missing_args_count} more positional arguments"))
            )
          ;; Check key arguments
          (while provided_args
            (let ( ( arg (pop provided_args) )
                   )
              (cond
                ;; It looks like a key argument is provided
                ( (and (symbolp arg) (equal ? (getchar arg 1)))
                  (cond
                    ;; Argument is unexpected
                    ( (not key_args[arg])
                      (unless (memq (car sexp) '( funcall apply makeInstance ))
                        (@lint_msg sexp messages levels 'WARNING 'EXTRA_KEY_ARG (@str "`{(car sexp)}` extra key argument {arg} is provided"))
                        )
                      (push arg remaining_args)
                      )
                    ;; Argument is expected and value is provided
                    ( provided_args (pop provided_args) )
                    ;; Argument is expected but has no value
                    ( t (@lint_msg sexp messages levels 'ERROR 'MISSING_KEY_VALUE (@str "`{(car sexp)} key argument {arg} requires a value`")) )
                    ))
                ( t (push arg remaining_args) )
                );cond
              ));let ;while
          (when (and remaining_args (not rest_arg))
            (@lint_msg sexp messages levels 'ERROR 'EXTRA_ARGS (@str "`{(car sexp)}` extra arguments are provided"))
            )
          ));let ;dbind
      ));fun

  (@fun get_arguments
    ( ( fun ?type symbol )
      )
    ?doc "Return advanced arguments associated to FUN.
Output is positional arguments, key arguments and rest argument."
    ?out ( list table list )|nil
    ?memoize t
    (let ( ( args_list            (@arglist fun)    )
           ( positional_arguments (tconc nil nil)   )
           ( key_arguments        (makeTable t nil) )
           ( rest_argument        ()                )
           ( kind                 '@pos             )
           )
      (unless (equal '( ( ___ ) ) args_list)
        (foreach argument args_list
          (@caseq (type argument)
            ( symbol (setq kind argument) )
            ( list
              (@caseq kind
                ( @pos  (tconc positional_arguments          argument) )
                ( @key  (setf key_arguments[(concat ? (car argument))] argument) )
                ( @rest
                  (assert (not rest_argument) "Function can have only one @rest argument.")
                  (setq rest_argument argument)
                  )
                ));caseq ;list
            ));caseq ;foreach
        (list (cdar positional_arguments) key_arguments rest_argument)
        );if
      ));let ;fun

  (@fun expand_macro
    ( ( sexp     ?type list  )
      ( messages ?type tconc )
      ( levels   ?type list  )
      )
    ?doc "Try to expand SEXP macro, raise messaages when failed"
    ?out list
    (@if (errset (expandMacro sexp) nil)
         ?var res
         ;; Return expanded macro
         (car res)
      ;; Fail to expand macro, raise Lint error and return nil
      (@lint_msg sexp messages levels 'ERROR 'MACRO_EXPANSION (@str "`{(car sexp)}` error when expanding macro: {errset.errset}"))
      nil
      ))

  );closure

(defun @lint_msg ( sexp messages levels type name msg @key ( predicate '@t ) @rest _)
  "Add Lint message to MESSAGES.
SEXP is the expression concerned by the message.
LEVELS contains the information about the nested parent expressions to reach the concerned SEXP.
TYPE describe message level (INFO, WARNING or ERROR).
NAME is the message reference."
  (tconc messages (list predicate type name levels msg sexp))
  nil)

(defun @lint_resolve_env ( sexp messages levels env )
  "Resolve ENV and report unused variables in SEXP."
  (let ( ( fun (car sexp) )
         )
    (foreach name env[?]
      (unless (eq '_ (getchar name 1))
        (@caseq env[name]->status
          ( ( used called global ) nil )
          ( assigned
            (@lint_msg sexp messages levels 'WARNING (concat (upperCase fun) '_ASSIGNED_ONLY)
              (@str "`{fun}` variable {name} is assigned only"))
            )
          ( t
            (@lint_msg sexp messages levels 'WARNING (concat (upperCase fun) '_UNUSED)
              (@str "`{fun}` variable {name} is unused")
              ;; Make sure function was not called before its definition (which is valid)
              ?predicate (let ( ( key name ) ) (lambda _ (neq 'used env[key]->status)))
              )
            )
          );caseq
        ));unless  ;foreach
    ));let ;fun

;; =======================================================
;; Rules Definitions
;; =======================================================

;; -------------------------------------------------------
;; Lint waiver
;; -------------------------------------------------------

(@macro @no_lint ( @rest body )
  "Lint waiver, equivalent to `progn'."
  (constar 'progn "NO_LINT" body))

(@lint_rule
  ?functions '( quote @no_lint )
  ?control t
  ?rule_fun '@nil
  )

;; -------------------------------------------------------
;; `progn`
;; -------------------------------------------------------

(@lint_rule
  ?functions '( progn )
  ?control t
  ?rule_fun
  (lambda ( sexp @rest args )
    (unless (equal "NO_LINT" (nth 1 sexp)) (apply '@lint_default_rule sexp args))
    ))

;; -------------------------------------------------------
;; `if`, `when`, `unless`
;; -------------------------------------------------------

(@lint_rule
  ?functions '( if )
  ?control t
  ?rule_fun
  (lambda ( sexp messages levels parents envs scheme )
    ;; TODO - Temporary support for (if <bool> then ... else ...)
    (@if (or (eq 'then (caddr sexp)) (not (cddddr sexp)))
         (let ( ( pos 0 )
                )
           (foreach sub_sexp (cdr sexp)
             pos++
             (unless (memq sub_sexp '( then else ))
               (@lint_sexp sub_sexp messages (cons pos levels) (cons sexp parents) envs scheme)
               )
             ))
      (@lint_msg sexp messages levels 'ERROR 'EXTRA_ARGS (@str "`if` extra args were provided: {(cddddr sexp)}"))
      )
    ))

(@lint_rule
  ?functions '( if )
  ?rule_fun
  (lambda ( sexp messages levels @rest _ )
    ;; (if <cond> nil ...) can be replaced by `unless`
    (unless (nth 2 sexp)
      (@lint_msg sexp messages levels 'INFO 'IF_NIL (@str "(if <cond> nil ...) can be replaced by (unless <cond> ...)"))
      )
    ;; (if <cond> <then>) can be replaced by `when`
    (unless (nthcdr 3 sexp)
      (@lint_msg sexp messages levels 'INFO 'IF_THEN (@str "(if <cond> <then>) can be replaced by (when <cond> <then>)"))
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
;; `setof`, `exists` & `forall`
;; -------------------------------------------------------

(@lint_rule
  ?functions '( setof exists forall )
  ?control t
  ?rule_fun
  (lambda ( sexp messages levels parents envs scheme )
    (unless
      (errset
        (destructuringBind ( fun var elts predicate ) sexp
          (@nif (symbolp var)
                (@lint_msg sexp messages levels 'ERROR (concat (upperCase fun) '_VARIABLE)
                  (@str "`{fun}` first argument should be an unquoted symbol")
                  )
            (let ( (env (makeTable t nil))
                   )
              (setf env[var] (list nil 'status 'unused))
              (@lint_sexp elts      messages (cons 2 levels) (cons sexp parents) envs            scheme)
              (@lint_sexp predicate messages (cons 3 levels) (cons sexp parents) (cons env envs) scheme)
              (@lint_resolve_env sexp messages levels env)
              )
            ))
        nil)
      (@lint_msg sexp messages levels 'ERROR (concat 'SYNTAX_ (upperCase (car sexp)))
        (@str "`{(car sexp)}` syntax should be ({(car sexp)} <variable> <list> <predicate>)"))
      )
    ))

;; -------------------------------------------------------
;; (car (setof ...)) to replace by (car (exists ...))
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
  (lambda ( sexp messages levels parents envs scheme )
    (cond
      ;; Check minumun number of arguments
      ( (not (cddr sexp)) (@lint_msg sexp messages levels 'ERROR 'CASE_MISSING_ARGS (@str "`{(car sexp)}` requires at least two arguments")) )
      ( t
        (destructuringBind ( fun val @rest cases ) sexp
          (let ( ( case_sexp_pos 1 )
                 )
            (@lint_sexp val messages (cons case_sexp_pos levels) (cons sexp parents) envs scheme)
            ;; Browse cases
            (foreach case cases
              case_sexp_pos++
              (cond
                ( (not (listp case))
                  (@lint_msg sexp messages levels 'ERROR 'SYNTAX_CASE (@str "`{fun}` argument should be a list: {case}"))
                  )
                ;; Parse result S-expressions
                ( t
                  (let ( ( sexp_pos 1 )
                         )
                    (foreach sub_sexp (cdr case)
                      (@lint_sexp sub_sexp messages
                        (constar sexp_pos++ case_sexp_pos levels  )
                        (constar case       sexp          parents )
                        envs scheme
                        )
                      ));foreach ;let
                  ) ;t
                ));cond ;foreach case
            ));let ;dbind
        ));t ;cond
    ))

;; -------------------------------------------------------
;; cond
;; -------------------------------------------------------

(@lint_rule
  ?functions '( cond )
  ?control t
  ?rule_fun
  (lambda ( sexp messages levels parents envs scheme )
    (prog ( ( cond_pos 0 )
            ( sexp_pos 0 )
            )
      (foreach tuple (cdr sexp)
        cond_pos++
        (setq sexp_pos 0)
        (unless (listp tuple)
          (@lint_msg sexp messages levels 'ERROR 'SYNTAX_CASE (@str "`{(car sexp)}` argument should be a list: {tuple}"))
          (return)
          )
        (foreach sub_sexp tuple
          (@lint_sexp sub_sexp messages
            (constar sexp_pos++ cond_pos levels  )
            (constar tuple      sexp     parents )
            envs scheme
            ))
        ))
    ))

;; -------------------------------------------------------
;; getq & getqq
;; -------------------------------------------------------

(@lint_rule
  ?functions '( getq getqq )
  ?control t
  ?rule_fun
  (lambda ( sexp messages levels parents envs scheme )
    (let ( ( fun (car   sexp) )
           ( obj (cadr  sexp) )
           ( key (caddr sexp) )
           )
      (@caseq fun
        ( getq  (@lint_sexp obj messages (cons 1 levels) (cons sexp parents) envs scheme) )
        ( getqq (unless (symbolp obj)
                  (@lint_msg sexp messages levels 'ERROR 'SYNTAX_GETQQ (@str "`{fun}` argument should be an unquoted symbol: {obj}"))))
        )
      (unless (symbolp key)
        (@lint_msg sexp messages levels 'ERROR 'SYNTAX_ (concat (upperCase fun))
          (@str "`{fun}` argument should be an unquoted symbol: {key}")
          ))
      )
    ))

(@lint_rule
  ?functions '( putpropq putpropqq )
  ?control t
  ?rule_fun
  (lambda ( sexp messages levels parents envs scheme )
    (let ( ( fun (car    sexp) )
           ( obj (cadr   sexp) )
           ( val (caddr  sexp) )
           ( key (cadddr sexp) )
           )
      (@caseq fun
        ( putpropq (@lint_sexp obj messages (cons 1 levels) (cons sexp parents) envs scheme) )
        ( putpropqq
          (if (symbolp obj)
              (@lint_msg sexp messages levels 'INFO 'GLOBAL (@str "`{fun}` global definition: {obj}.{key}"))
            (@lint_msg sexp messages levels 'ERROR 'SYNTAX_PUTPROPQQ (@str "`{fun}` argument should be an unquoted symbol: {obj}"))
            ))
        )
      (unless (symbolp key)
        (@lint_msg sexp messages levels 'ERROR 'SYNTAX_ (concat (upperCase fun))
          (@str "`{fun}` argument should be an unquoted symbol: {key}")
          ))
      (@lint_sexp val messages (cons 2 levels) (cons sexp parents) envs scheme)
      )
    ))

;; -------------------------------------------------------
;; let
;; -------------------------------------------------------

(@lint_rule
  ?functions '( let letseq prog )
  ?control t
  ?rule_fun
  (lambda ( sexp messages levels parents envs scheme )
    (cond
      ;; No arguments, this is already reported when checking arguments
      ( (not (cdr sexp)) )
      ( t
        (destructuringBind ( fun defs @rest body ) sexp
          (let ( ( env      (makeTable t nil) )
                 ( sexp_pos 2                 )
                 )
            ;; In `letseq` current environment is already available for next definitions
            (when (eq fun 'letseq) (setq envs (cons env envs)))
            (@nif (listp defs)
                  (@lint_msg sexp messages levels 'ERROR (concat 'SYNTAX_ (upperCase fun))
                    (@str "`{fun}` first argument should be a list: {defs}"))
              ;; Define each variable in environment
              (let ( ( def_pos -1 )
                     name
                     )
                (foreach def defs
                  def_pos++
                  ;; Make sure variable definition is valid
                  (setq name nil)
                  (caseq (type def)
                    ( symbol (setq name def) )
                    ( list
                      (@nif (and (symbolp (car def)) (cdr def) (not (cddr def)))
                            ;; Definition is wrong, report it
                        (@lint_msg sexp messages (constar def_pos 1 levels) 'ERROR (concat 'SYNTAX_ (upperCase fun) '_BINDING)
                          (@str "`{fun}` binding must be a symbol or symbol-value pair: {def}"))
                        (setq name (car def))
                        ;; Check variable definition
                        (@lint_sexp (cadr def) messages
                          (constar 1   def_pos 1    levels )
                          (constar def defs    sexp parents)
                          envs scheme
                          )
                        ));nif ;list
                    (t
                      ;; Variable definition should be a symbol or a symbol-value pair
                      (@lint_msg sexp messages (constar def_pos 1 levels) 'ERROR (concat 'SYNTAX_ (upperCase fun) '_BINDING)
                        (@str "`{fun}` binding must be a symbol or symbol-value pair: {def}"))
                      ));t ;caseq
                  ;; Add variable to env
                  ;; Report superseded variable
                  (when (and (symbolp name) (neq '_ (getchar name 1)) (exists env envs env[name]))
                    (@lint_msg sexp messages (constar def_pos 1 levels) 'WARNING (concat (upperCase fun) '_SUPERSEDE)
                      (@str "`{fun}` variable {name} is superseded")
                      ))
                  ;; Report two variables having the same name in the same let
                  (cond
                    ( (not name) )
                    ( env[name]
                      (@lint_msg sexp messages (constar def_pos 1 levels) 'WARNING (concat (upperCase fun) '_UNREACHABLE_VAR)
                        (@str "`{fun}` another variable is already called {name}")) )
                    (t (setf env[name] (list nil 'status 'unused)) )
                    )
                  ));foreach ;let
              (push env envs)
              ;; Check body
              (foreach sub_sexp body
                (@lint_sexp sub_sexp messages (cons sexp_pos++ levels) (cons sexp parents) envs scheme)
                )
              ;; Check unused variables
              (@lint_resolve_env sexp messages levels env)
              );nif
            ));let ;dbind
        ));t ;cond
    ))

;; -------------------------------------------------------
;; `set` & `setq`
;; -------------------------------------------------------

(@lint_rule
  ?functions '( set setq )
  ?control t
  ?rule_fun
  (lambda ( sexp messages levels parents envs scheme )
    (unless
      (errset
        (destructuringBind ( fun name value @optional ( env '__UNDEFINED__ ) ) sexp
          (cond
            ;; Treat `setq` and (set (quote <name>) ...) the same way
            ( (or (eq fun 'setq)
                  (and (eq fun 'set)
                       (listp name)
                       (eq 'quote (car name))
                       (not (cddr name))
                       (symbolp (cadr name))
                       (setq name (cadr name))
                       ))
              (@if (exists env envs (and (tablep env) env[name]))
                   ?var res_envs
                   ;; Assigned is a special status where variable appears as used but is in fact "only-assigned"
                   (when (eq 'unused (car res_envs)[name]->status)
                     (setf (car res_envs)[name]->status 'assigned))
                (cond
                  ;; SKILL definition
                  ( (and (not scheme) (eq env '__UNDEFINED__))
                    (@lint_msg sexp messages levels 'WARNING 'GLOBAL (@str "`{fun}` global definition: {name}"))
                    )
                  ;; Function defintion
                  ( (and (listp value) (symbolp (car value)) (memq (@output (car value)) '( function funobj callable )))
                    (@lint_msg sexp messages levels 'WARNING 'GLOBAL (@str "`{fun}` global function definition: {name}"))
                    )
                  ;; Scheme definition
                  ( t (@lint_msg sexp messages levels 'WARNING 'GLOBAL (@str "`{fun}` global scheme definition: {name}")) )
                  ))
              ;; Check value
              (@lint_sexp value messages (cons 2 levels) (cons sexp parents) envs scheme)
              )
            ;; Apply default rule otherwise
            (t (@lint_default_rule sexp messages levels parents envs scheme))
            ));cond ;dbind
        nil);errset
      (@lint_msg sexp messages levels 'ERROR (concat 'SYNTAX_ (upperCase (car sexp)))
        (@str "`{(car sexp)}` syntax must be ({(car sexp)} <name> <value>)"))
      )
    ))

;; -------------------------------------------------------
;; Functions definition
;; -------------------------------------------------------

(@lint_rule
  ?functions '( define procedure globalProc defun defglobalfun defmacro defmethod lambda )
  ?control t
  ?rule_fun
  (lambda ( sexp messages levels parents envs scheme )
    (prog ( ( fun  (car sexp) )
            ( args (cdr sexp) )
            name
            bindings
            bindings_levels
            bindings_pos
            body
            body_pos
            )
      (cond

        ;; `define` used like `setq`
        ( (and (eq 'define fun) (symbolp (setq name (car args))) )
          (cond
            ;; Wrong syntax
            ( (or (not (cdr args)) (cddr args))
              (@lint_msg sexp messages levels 'ERROR 'SYNTAX_DEFINE
                (@str "`{fun}` syntax must be ({fun} <name> <value>) or ({fun} ( <name> <args>... ) <body>...)")
                )
              (return)
              )
            ;; Local variable
            ( (and scheme (tablep (car envs))) (setf (car envs)[name] (list nil 'status 'unused)) )
            ;; Global variable
            ;; In SKILL, `define` can only be used to define global functions
            ( t
              (if (and (listp (cadr args))
                       (symbolp (caadr args))
                       (memq (@output (caadr args)) '( function funobj callable ))
                       )
                  (@lint_msg sexp messages levels 'INFO 'GLOBAL (@str "`{fun}` global function definition: {name}"))
                (@lint_msg sexp messages levels 'INFO 'GLOBAL (@str "`{fun}` global definition: {name}"))
                ))
            );cond
          ;; Check assigned value S-expression
          (@lint_sexp (cadr args) messages (cons 2 levels) (cons sexp parents) envs scheme)
          (return)
          )

        ;; `define`, `procedure`, `globalProc`
        ( (memq fun '( define procedure globalProc ))
          (unless (and (listp   (car  args))
                       (symbolp (caar args))
                       (cdr args)
                       )
            ;; Invalid syntax
            (@lint_msg sexp messages levels 'ERROR (concat 'SYNTAX_ (upperCase fun))
              (@str "`{fun}` syntax must be ({fun} ( <name> <args> ... ) <body> ...)")
              )
            (return)
            )
          ;; Valid syntax, fetch name, bindings and body
          (setq name     (caar args))
          (setq bindings (cdar args))
          (setq body     (cdr  args))
          ;; Define bindings_levels and bindings_pos
          (setq bindings_levels (cons 1 levels))
          (setq bindings_pos    1              )
          (setq body_pos        2              )
          )

        ;; `defun`, `defglobalfun`
        ( (memq fun '( defun defglobalfun defmacro defmethod ))
          (@caseq fun
            ( ( defun defglobalfun )
              ;; Support symbol as arguments
              (when (symbolp (cadr args))
                (setf (cadr args) (list '@rest (cadr args)))
                ) )
            ( ( defmacro  ) (setq scheme nil) )
            ( ( defmethod ) ()                )
            )
          ;; Check syntax
          (unless (and (symbolp (car  args))
                       (cddr args)
                       ;; Support method  combination arguments
                       (or (listp (cadr args))
                           (and (eq fun 'defmethod)
                                (memq (cadr args) '( @before @after @around ))
                                (listp (caddr args))
                                (cdddr args)
                                ))
                       )
            ;; Invalid syntax
            (@lint_msg sexp messages levels 'ERROR (concat 'SYNTAX_ (upperCase fun))
              (@str "`{fun}` syntax must be ({fun} <name> ( <args> ... ) <body> ...)")
              )
            (return)
            )
          (setq name (car args))
          (cond
            ;; Support method combination argument when provided
            ( (and (eq fun 'defmethod) (memq (cadr args) '( @before @after @around )) )
              ;; Fetch name, bindings and body
              (setq bindings (caddr args))
              (setq body     (cdddr args))
              ;; Define bindings_levels and bindings_pos
              (setq bindings_levels (cons 3 levels))
              (setq bindings_pos    0              )
              (setq body_pos        4              )
              )
            ( t
              ;; Valid syntax, fetch name, bindings and body
              (setq bindings (cadr args))
              (setq body     (cddr args))
              ;; Define bindings_levels and bindings_pos
              (setq bindings_levels (cons 2 levels))
              (setq bindings_pos    0              )
              (setq body_pos        3              )
              ))
          )

        ;; `lambda`
        ( (eq fun 'lambda)
          ;; Support symbol as arguments
          (when (symbolp (car args))
            (setf (car args) (list '@rest (car args)))
            )
          ;; Check syntax
          (unless (and (listp (car args))
                       (cdr args)
                       )
            (@lint_msg sexp messages levels 'ERROR (concat 'SYNTAX_ (upperCase fun))
              (@str "`{fun}` syntax must be ({fun} ( <args> ... ) <body> ...)")
              )
            (return)
            )
          (setq bindings (car args))
          (setq body     (cdr args))
          ;; Define bindings_levels and bindings_pos
          (setq bindings_levels (cons 1 levels))
          (setq bindings_pos    0              )
          (setq body_pos        2              )
          )

        ;; Rest is unsupported and should never happen
        ( t (error "This is never supposed to occur...") )
        );cond

      ;; Check if function supersedes another one
      (when (and name (exists env envs (and (tablep env) env[name])))
        (unless (and (eq fun 'defmethod) (isGeneric name))
          (@lint_msg sexp messages levels 'WARNING (concat (upperCase fun) '_SUPERSEDE)
            (@str "`{fun}` variable {name} is superseded")
            )))
      ;; Define variable
      (cond
        ( (not name) (assert (eq fun 'lambda) "Only `lambda` should not be defining a variable") )
        ( (tablep (car envs))
          (cond
            ( (or (not scheme)
                  (memq fun '( globalProc defglobalfun defmacro defmethod ))
                  )
              ;; Definition is local and global
              (setf (car envs)[name] (list nil 'status 'global))
              (@lint_msg sexp messages levels 'INFO 'GLOBAL (@str "`{fun}` global function definition: {name}"))
              )
            ( t
              ;; Definition is local only
              (setf (car envs)[name] (list nil 'status 'unused 'type 'function))
              )
            ))
        ( t (@lint_msg sexp messages levels 'INFO 'GLOBAL (@str "`{fun}` global function definition: {name}")) )
        );cond

      ;; Make sure bindings syntax is valid, define new_env using bindings
      (let ( ( env         (makeTable t nil)   )
             ( binding_pos (sub1 bindings_pos) )
             binding
             arg_name
             )
        ;; Parse method specific arguments
        (when (eq 'defmethod fun)
          (while (and bindings (listp (car bindings)))
            (setq binding (pop bindings))
            binding_pos++
            (@nif (and (listp binding)
                       (symbolp (car binding))
                       (symbolp (cadr binding))
                       (not (cddr binding))
                       )
                  (@lint_msg sexp messages levels 'ERROR 'SYNTAX_DEFMETHOD
                    "`defmethod` syntax is (defmethod <name> ( ( <arg_name> <arg_class> ) ... ) <body> ...)")
              (setf env[(car binding)] (list nil 'status 'unused))
              )))

        ;; Parse arguments common to all functions
        (foreach map sub_binding bindings
          (setq binding (car sub_binding))
          binding_pos++
          (setq arg_name nil)
          (cond
            ( (memq binding '( @optional @rest @key @aux )) )
            ( (symbolp binding) (setq arg_name binding) )
            ( (and (listp binding) (symbolp (car binding)) (cdr binding) (not (cddr binding)))
              (setq arg_name (car binding))
              ;; Check default value
              (@lint_sexp (cadr binding) messages
                (constar 1 binding_pos bindings_levels)
                (constar binding bindings sexp parents)
                (if scheme (cons env envs) envs)
                scheme
                ))
            ;; Final string in the argument list
            ( (and (stringp binding) (not (cdr sub_binding)))
              ;; TODO - check arguments type-checking string content

              )
            ( t
              (@lint_msg sexp messages (constar binding_pos bindings_levels) 'ERROR (concat 'SYNTAX_ (upperCase fun) '_BINDING)
                (@str "`{fun}` binding must be a symbol or symbol-value pair: {binding}"))
              ));t ;cond
          (cond
            ( (not arg_name) )
            ( env[arg_name]
              (@lint_msg sexp messages (constar binding_pos bindings_levels) 'WARNING (concat (upperCase fun) '_UNREACHABLE_VAR)
                (@str "`{fun}` another argument is already called {arg_name}")) )
            ( t (setf env[arg_name] (list nil 'status 'unused)) )
            )
          );foreach
        ;; Check docstring
        (and name
             (neq 'defmacro fun)
             (not (stringp (car body)))
             (@lint_msg sexp messages levels 'WARNING 'MISSING_DOCSTRING (@str "`{fun}` {name} has no docstring"))
             )
        ;; Check body
        (foreach sub_sexp body
          (@lint_sexp sub_sexp messages (cons body_pos++ levels) (cons sexp parents) (cons env envs) scheme)
          );foreach
        ;; Check unused variables
        (@lint_resolve_env sexp messages levels env)
        );let
      );prog
    ))


;; -------------------------------------------------------
;; foreach
;; -------------------------------------------------------

(@lint_rule
  ?functions '( foreach )
  ?control t
  ?rule_fun
  (lambda ( sexp messages levels parents envs scheme )
    (prog ( ( fun      (car sexp) )
            ( body     (cdr sexp) )
            ( sexp_pos 1          )
            env
            names
            )
      (when (memq (car body) '( map mapcar mapcan mapc maplist mapcon ))
        (pop body)
        sexp_pos++
        )
      (cond
        ( (symbolp (car body))                                             (setq names (list (car body))) )
        ( (and (listp (car body)) (forall name (car body) (symbolp name))) (setq names (car body))        )
        ( t
          (@lint_msg sexp messages levels 'ERROR 'SYNTAX_FOREACH (@str "`{fun}` syntax is (foreach [map_fun] <name> <list> <body>...)"))
          (return)
          ))
      (progn (pop body) sexp_pos++)
      ;; Parse list definitions
      (setq env (makeTable t nil))
      (foreach name names
        (@lint_sexp (pop body) messages (cons sexp_pos++ levels) (cons sexp parents) envs scheme)
        (setf env[name] (list 'status 'unused))
        )
      ;; Parse body
      (foreach sub_sexp body
        (@lint_sexp sub_sexp messages (cons sexp_pos++ levels) (cons sexp parents) (cons env envs) scheme)
        )
      (@lint_resolve_env sexp messages levels env)
      )
    ))

;; -------------------------------------------------------
;; for
;; -------------------------------------------------------

(@lint_rule
  ?functions '( for )
  ?control t
  ?rule_fun
  (lambda ( sexp messages levels parents envs scheme )
    (prog ( ( fun  (car    sexp) )
            ( var  (cadr   sexp) )
            ( beg  (caddr  sexp) )
            ( end  (cadddr sexp) )
            ( body (cddddr sexp) )
            ( env  (makeTable t nil))
            ( sexp_pos 2 )
            )
      (unless (symbolp var)
        (@lint_msg sexp messages levels 'ERROR 'SYNTAX_FOR (@str "`{fun}` syntax is (for <name> <beg> <end> <body>...)"))
        (return)
        )
      (setf env[var] (list 'status 'unused))
      (@lint_sexp beg messages (cons sexp_pos++ levels) (cons sexp parents) envs scheme)
      (@lint_sexp end messages (cons sexp_pos++ levels) (cons sexp parents) envs scheme)
      (foreach sub_sexp body
        (@lint_sexp sub_sexp messages (cons sexp_pos++ levels) (cons sexp parents) (cons env envs) scheme)
        )
      (@lint_resolve_env sexp messages levels env)
      )
    ))

;; -------------------------------------------------------
;; prog
;; -------------------------------------------------------

;; TODO - Rules for `prog`
;; TODO - Report s-expressions after `return` or `go`

;; -------------------------------------------------------
;; inScheme, inSkill & dynamic
;; -------------------------------------------------------

(@lint_rule
  ?functions '( inSkill inScheme )
  ?control t
  ?rule_fun
  (lambda ( sexp messages levels parents envs _scheme )
    (let ( ( pos 1 )
           )
      (foreach sub_sexp (cdr sexp)
        (@lint_sexp sub_sexp messages (cons pos++ levels) (cons sexp parents) envs (@caseq (car sexp) ( inSkill nil ) ( inScheme t )))
        ))
    ))

(@lint_rule
  ?functions '( dynamic )
  ?control t
  ?rule_fun
  (lambda ( sexp messages levels parents envs _scheme )
    (@nif (symbolp (cadr sexp))
          (@lint_msg sexp messages levels 'ERROR 'SYNTAX_DYNAMIC (@str "`{(car sexp)}` argument should be a symbol: {(cadr sexp)}"))
      (@lint_sexp (cadr sexp) messages (cons 1 levels) (cons sexp parents) envs nil)
      )
    ))

;; -------------------------------------------------------
;; Unknown `status` or `sstatus` calls
;; -------------------------------------------------------

(@lint_rule
  ?functions '( status sstatus )
  ?control t
  ?rule_fun
  (lambda ( sexp messages levels parents @rest args )
    ;; Static condition
    (unless (errset (funcall 'status (nth 1 sexp)))
      (@lint_msg sexp messages levels 'WARNING 'STATUS_FLAG (@str "`{(car sexp)}` unknown flag: {(nth 1 sexp)}"))
      )
    ;; Parse remaining arguments
    (let ( ( sexp_pos 2 )
           )
      (foreach sub_sexp (nthcdr 2 sexp)
        (apply '@lint_sexp sub_sexp messages (cons sexp_pos++ levels) (cons sexp parents) args)
        ));foreach ;let
    ))

;; -------------------------------------------------------
;; defclass
;; -------------------------------------------------------

;; TODO - defclass is waived for now
(@lint_rule
  ?functions '( defclass @class )
  ?control t
  ?rule_fun '@nil
  )

;; -------------------------------------------------------
;; Anaphoric macros
;; -------------------------------------------------------

(@lint_rule
  ?functions '( @if @nif @when )
  ?control t
  ?rule_fun
  (lambda ( sexp messages levels parents envs scheme )
    (let ( ( body     (cdr sexp) )
           ( env      (makeTable t nil) )
           ( name     (cadr (exists sub_sexp (cdr sexp) (eq '?var sub_sexp))) )
           ( sexp_pos 0 )
           sub_sexp
           )
      ;; Add environment when name is defined
      (when name
        (setf env[name] (list nil 'status 'unused))
        (push env envs)
        )
      (while body
        sexp_pos++
        (setq sub_sexp (pop body))
        (cond
          ( (eq '?var sub_sexp) sexp_pos++ (setq sub_sexp (pop body)) )
          ( t (@lint_sexp sub_sexp messages (cons sexp_pos levels) (cons sexp parents) envs scheme))
          ));cond ;while
      (when name (@lint_resolve_env sexp messages levels env))
      );let
    ))

;; -------------------------------------------------------
;; With
;; -------------------------------------------------------

(@lint_rule
  ?functions '( @with )
  ?control   t
  ?rule_fun
  (lambda ( sexp messages levels parents envs scheme )
    (prog ( ( fun         (car sexp)        )
            ( bindings    (cadr sexp)       )
            ( body        (cddr sexp)       )
            ( sexp_pos    1                 )
            ( binding_pos 0                 )
            ( env         (makeTable t nil) )
            )
      (unless (listp bindings)
        (@lint_msg sexp messages levels 'ERROR 'SYNTAX_WITH (@str "`{fun}` syntax is (@with ( ( <name> <value> ) ... ) <body> ...)"))
        (return)
        )
      ;; Parse bindings
      (foreach binding bindings
        (unless (and (symbolp (car binding))
                     (cdr binding)
                     (not (cddr binding))
                     )
          (@lint_msg sexp messages levels 'ERROR 'SYNTAX_WITH (@str "`{fun}` syntax is (@with ( ( <name> <value> ) ... ) <body> ...)"))
          (return)
          )
        (@lint_sexp (cadr binding) messages (constar 1 binding_pos++ sexp_pos++ levels) (constar binding bindings sexp parents) (cons env envs) scheme)
        (setf env[(car binding)] (list nil 'status 'unused))
        )
      sexp_pos++
      ;; Parse body
      (foreach sub_sexp body
        (@lint_sexp sub_sexp messages (cons sexp_pos++ levels) (cons sexp parents) (cons env envs) scheme)
        )
      (@lint_resolve_env sexp messages levels env)
      );prog
    ))

;; -------------------------------------------------------
;; Wrap
;; -------------------------------------------------------

(@lint_rule
  ?functions '( @wrap )
  ?control   t
  ?rule_fun
  (lambda ( sexp messages levels parents envs scheme )
    (let ( ( sexp_pos 1 )
           )
      (unless (or (cadr sexp) (caddr sexp))
        (@lint_msg sexp messages levels 'INFO 'EXTRA_WRAP "`@wrap` without IN or OUT can be removed or replaced by `progn`")
        )
      (foreach sub_sexp (cdr sexp)
        (@lint_sexp sub_sexp messages (cons sexp_pos++ levels) (cons sexp parents) envs scheme)
        ))
    ))

;; -------------------------------------------------------
;; Fun
;; -------------------------------------------------------

(@lint_rule
  ?functions '( @fun )
  ?control   t
  ?rule_fun
  (lambda ( sexp messages levels parents envs scheme )
    (prog ( ( fun         (car   sexp)      )
            ( name        (cadr  sexp)      )
            ( bindings    (caddr sexp)      )
            ( body        (cdddr sexp)      )
            ( sexp_pos    2                 )
            ( binding_pos -1                )
            ( env         (makeTable t nil) )
            sub_sexp
            )
      (unless (and (symbolp name) (listp bindings))
        (@lint_msg sexp messages levels 'ERROR 'SYNTAX_FUN
          "`@fun` syntax is (@fun <name> ( ( <arg_name> [?def <value>] [?type <type>] ... ) ... ) ?doc <doc> <body> ...)")
        (return)
        )

      ;; Check if function supersedes another one
      (when (and name (exists env envs (and (tablep env) env[name])))
        (@lint_msg sexp messages levels 'WARNING (concat (upperCase fun) '_SUPERSEDE)
          (@str "`{fun}` variable {name} is superseded")
          ))
      ;; Define variable
      (cond
        ( (tablep (car envs))
          (@nif (cadr (memq '?global body))
                ;; Definition is local only
                (setf (car envs)[name] (list nil 'status 'unused 'type 'function))
            ;; Definition is local and global
            (setf (car envs)[name] (list nil 'status 'global 'type 'function))
            (@lint_msg sexp messages levels 'INFO 'GLOBAL (@str "`{fun}` global definition: {name}"))
            ))
        ( t (@lint_msg sexp messages levels 'INFO 'GLOBAL (@str "`{fun}` global definition: {name}")) )
        );cond

      ;; Parse bindings
      (foreach binding bindings
        binding_pos++
        (cond
          ( (memq binding '( @key @rest _ )) nil )
          ( (not (and (listp binding) (symbolp (car binding))))
            (@lint_msg sexp messages (cons binding_pos levels) 'ERROR 'SYNTAX_FUN
              (@str "`{fun}` binding format is ( <arg_name> [?def <value>] [?type <type>] ... ): {binding}")
              )
            (return)
            )
          (t
            (let ( ( arg_name (pop binding) )
                   ( pos      0             )
                   )
              (while binding
                (setq sub_sexp (pop binding))
                pos++
                (cond
                  ;; Binding is ?type
                  ( (eq sub_sexp '?type)
                    (setq sub_sexp (pop binding))
                    pos++
                    ;; TODO - Parse binding ?type

                    )
                  ;; Binding is ?def or ?doc
                  ( (memq sub_sexp '( ?def ?doc )) nil )
                  ;; Binding is ?...
                  ( (and (symbolp sub_sexp) (eq '? (getchar sub_sexp 1)))
                    (@lint_msg sexp messages (cons binding_pos levels) 'INFO 'EXTRA_KEY_ARG
                      (@str "`{fun}` extra key argument {sub_sexp} in binding {binding}")
                      ))
                  (t
                    ;; Parse sexp
                    (@lint_sexp sub_sexp messages
                      (constar pos     binding_pos sexp_pos levels )
                      (constar binding bindings    sexp     parents)
                      (if scheme (cons env envs) env)
                      scheme
                      ))
                  ));cond ;while
              ;; Add argument name to environment
              (setf env[arg_name] (list nil 'status 'unused))
              ));let ;t
          ));cond ;foreach binding
      ;; Parse body
      (while body
        (setq sub_sexp (pop body))
        sexp_pos++
        (cond
          ;; TODO - Specific parsing for ?out
          ( (eq sub_sexp '?out)
            (setq sub_sexp (pop body))
            sexp_pos++

            )
          ( (memq sub_sexp '( ?doc ?global ?memoize )) nil )
          ;; Parse any other sexp
          ( t (@lint_sexp sub_sexp messages (cons sexp_pos levels) (cons sexp parents) (cons env envs) scheme))
          ));cond ;while
      (@lint_resolve_env sexp messages levels env)
      );prog
    ))

;; -------------------------------------------------------
;; Debugging functions
;; -------------------------------------------------------

(@lint_rule
 ?functions '( break breakpt breakptMethod
               clear cont continue count
               debugQuit debugStatus dump
               gcsummary getAllLoadedFiles getCallingFunction getFunctions getGFbyClass
               ilAddTopLevelErrorHandler ilDebugCountLevels ilGetGFbyClass ilGetIdeSessionWindow ilGetTCovFiles ilMergeTCovData
               ilRemoveMethod ilRemoveTopLevelErrorHandler ilSlotBoundp ilToolBox inNext inStepOut installDebugger
               listAlias listFunctions listVariables
               memoryAllocated
               next
               pp printFunctions printObject printstruct printVariables
               removeMethod resume
               skillDebugger skillDevStatus stacktrace step stepend stepout
               toplevel tracef tracelevlimit tracelevunlimit tracep tracev
               unbreakpt unbreakptMethod uncount uninstallDebugger untrace untracep untracev unwatch
               watch where whereIs
               ;; Custom
               @show @print_args @print_table @runtime
               )
 ?rule_fun
 (lambda ( sexp messages levels @rest _ )
   (@lint_msg sexp messages levels (if (@get_debug) 'INFO 'WARNING) 'DEBUGGING
     (@str "`{(car sexp)}` debugging function should not be used in production")
     ))
 )


;; =======================================================
;; Apply rules
;; =======================================================

(let ()

  (@fun @lint
    ( @key
      ( files      ?type ( string ... )                                                         )
      ( info_port  ?type port           ?def (@poport)                                          )
      ;; warn_port defaults to stderr using SKILL Interpreter but woport otherwise
      ( warn_port  ?type port           ?def (if (eq (@poport) (@woport)) (@errport) (@woport)) )
      ( err_port   ?type port           ?def (@errport)                                         )
      ( filters
        ?type ( symbol ... )
        ?def  (mapcar 'concat (parseString (or (getShellEnvVar "SKILL_SHARP_LINT_FILTERS") "") ","))
        ?doc  "Only print infos, warnings and errors that matches exactly words in this comma-separated value"
        )
      ( ignores
        ?type ( symbol ... )
        ?def  (mapcar 'concat (parseString (or (getShellEnvVar "SKILL_SHARP_LINT_HIDE_IGNORES") "") ","))
        ?doc  "Waive infos, warnings and errors whose names matches exactly words in this comma-separated value"
        )
      ( hide_sexps
        ?type t|nil
        ?def  (equal "TRUE" (getShellEnvVar "SKILL_SHARP_LINT_HIDE_SEXPS"))
        ?doc  "Do not print S-expressions where error occured"
        )
      ( no_header
        ?type t|nil
        ?def  nil
        ?doc  "When non-nil, header and output status are omitted."
        )
      )
    ?doc "Run Sharper Lint on FILES.
All report messages are printed to PORT."
    ?out t|nil
    ?global t
    (assert files "@lint - ?files is nil")
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
                 ( scheme              (pcreMatchp "\\.(ils|scm)$" file))
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
                      (@lint_sexp sexp messages (list sexp_pos++) sexps nil scheme)
                      );foreach
                    (setq messages (cdar messages))
                    ;; Filter messages whose predicate does not pass
                    (setq messages
                      (foreach mapcan message messages
                        (when (funcall (car message)) (list (cdr message)))
                        ))
                    ;; Find actual lines where messages occured
                    (setq messages
                      (foreach mapcar message messages
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
        (unless no_header (@fprintf info_port "Running Lint - {(getCurrentTime)}\n"))
        (@foreach_dbind ( file res )  (cdar results_by_file)
          (unless no_header (@fprintf info_port "File {file}:\n"))
          (@foreach_dbind ( _beg_pos ( _beg_line _end_line ) messages ) res
            ;; No need to print where top-level S-expressions are found
            ;; Only the info, warnings and errors matter
            ; (when messages
            ;   (if (eq beg_line (sub1 end_line))
            ;       (@fprintf info_port "  Top-level S-Expression at line {beg_line}:\n")
            ;     (@fprintf info_port "  Top-level S-Expression at lines {beg_line} - {(sub1 end_line)}:\n")
            ;     ))
            (@foreach_dbind ( type name line text sexp ) messages
              (cond
                ( (memq name ignores)                     )
                ( (and filters (not (memq name filters))) )
                ( t
                  (let ( port )
                    (@caseq type
                      ( INFO    (setq port info_port)                        )
                      ( WARNING (setq port warn_port) (setq lint_status nil) )
                      ( ERROR   (setq port err_port ) (setq lint_status nil) )
                      );caseq
                    (if (or hide_sexps (eq 'GLOBAL name))
                        (@fprintf port "  {type%7s} {name%s} at line {line%-3d} - {text}\n")
                      (@fprintf port "  {type%7s} {name%s} at line {line%-3d} - {text} - {sexp}\n")
                      ));if ;let
                  ));t ;cond
              ));foreach_dbind message ;foreach_dbind lines
          (unless no_header (newline info_port))
          );foreach_dbind file
        (unless no_header (println (if lint_status 'PASS 'FAIL) info_port))
        ;; Return status
        lint_status
        );letf
      ));let ;fun

  (defun count_lines ( port beg_pos end_pos )
    "Count the newlines between BEG_POS and END_POS in PORT."
    (fileSeek port beg_pos 0)
    (let ( ( lines 0 )
           )
      (for _i beg_pos end_pos-1
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
        (while (and levels (neq 'STOP (car levels)))
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
            (when (and levels (neq 'STOP (car levels)))
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

