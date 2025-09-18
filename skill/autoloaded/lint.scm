;; ===============================================================================================================
;; Fully custom Lint as native one was too buggy and not suitable for SKILL++ and SKILL#.
;; 
;; A. Buchet - September 2025
;; ===============================================================================================================

;; DEBUG
;(@set_debug t)

;; =======================================================
;; Fixes arguments check for macros and syntax forms
;; =======================================================

;; TODO - Contact Cadence support about (arglist 'defun) and (arglist 'if)

;; No idea why but (arglist 'if) mentions @rest for else part
(setf (@arglist 'if)
  '( ( g_general  ?type general          )
     ( g_general  ?type general          )
     ( g_general  ?type general ?def nil )
     ))

;; No idea why but (arglist 'defun) does not mention body
(setf (@arglist 'defun)
  '( ( s_symbol  ?type symbol  )
     ( g_general ?type general )
     ( g_general ?type general )
     @rest
     ( body ?type ( general ... ) )
     ))

(setf (@arglist 'defglobalfun) (@arglist 'defun))

;; =======================================================
;; Add Rule
;; =======================================================

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
          ( (exists env envs (and (tablep env) env[fun]))
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
      (_\@lint_sexp sub_sexp messages (cons sexp_pos++ levels) (cons sexp parents) envs scheme)
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

  (@fun lint_get_rules
    ( ( fun ?type symbol )
      )
    ?doc "Return rules associated to FUN."
    ?out ( callable ... )
    (cons control_rule_by_fun[fun] rules_by_fun[fun])
    )

  (defglobalfun _\@lint_sexp ( sexp messages levels parents envs scheme )
    "Lint SEXP."
    ;(@debug "_@lint_sexp\n  sexp: {sexp}\n  messages: {messages}\n\n")
    ;; TODO - At least in debug mode, assert that levels and parents are matching (in terms of nested expressions)
    
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
            ;; Variable is bound
            ( (if scheme (boundp sexp (schemeTopLevelEnv)) (boundp sexp))
              (@lint_msg sexp messages levels 'WARNING 'GLOBAL_USE "Global variable is used")
              )
            ( t (@lint_msg sexp messages levels 'ERROR 'GLOBAL_USE "Undefined global variable is used") )
            );cond
          ))
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
              ;; Find first environment where variable is used
              (@when (car (exists env envs (and (tablep env) env[fun])))
                ?var env
                (setf env[fun]->status 'called)
                )
              ;; TODO - If variable is in an environment, make sure it is callable and check its arguments if they are defined
              
              ;; Function is global, check its arguments and its rules
              (@nif (getd fun)
                    (@lint_msg sexp messages levels 'WARNING 'NOT_CALLABLE (@str "Function {fun} is not callable"))
                (unless (errset (check_arguments sexp messages levels (get_arguments fun)) nil)
                  (@lint_msg sexp messages levels 'WARNING 'ARGS_CHECK_FAIL (@str "Error when checking arguments: {errset.errset}"))
                  )
                (foreach rule (lint_get_rules fun)
                  (unless (errset (funcall rule sexp messages levels parents envs scheme) nil)
                    (@lint_msg sexp messages levels 'WARNING 'LINT_ERROR (@str "Error when applying `{fun}` rule: {errset.errset}"))
                    ))
                ))
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
                    (eq ? (getchar arg 1))
                    (neq 'quote (car sexp))
                    (@lint_msg sexp messages levels 'WARNING 'POSITIONAL_KEY_ARG (@str "`{(car sexp)}` argument {arg} is treated as positional, move or quote it for disambiguation"))
                    )
                  ))
              ;; Missing argument, check if argument is required or optional
              ( (memq ?def positional_arg->?) )
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
                      (@lint_msg sexp messages levels 'WARNING 'EXTRA_KEY_ARG (@str "`{(car sexp)}` extra key argument {arg} is provided"))
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

  );closure


(defun @lint_msg ( sexp messages levels type name msg )
  "Add Lint message to MESSAGES.
SEXP is the expression concerned by the message.
LEVELS contains the information about the nested parent expressions to reach the concerned SEXP.
TYPE describe message level (INFO, WARNING or ERROR).
NAME is the message reference."
  (tconc messages (list type name levels msg sexp))
  nil)

(defun @lint_resolve_env ( sexp messages levels env )
  "Resolve ENV and report unused variables in SEXP."
  (let ( ( fun (car sexp) )
         )
    (foreach name env[?]
      (@caseq env[name]->status
        ( ( used called global ) nil )
        ( assigned
          (@lint_msg sexp messages levels 'WARNING (concat (upperCase fun) '_ASSIGNED_ONLY)
            (@str "`{fun}` variable {name} is assigned only"))
          )
        ( t
          (@lint_msg sexp messages levels 'WARNING (concat (upperCase fun) '_UNUSED)
            (@str "`{fun}` variable {name} is unused"))
          )
        ));caseq ;foreach
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

(SK_RULE SK_CONTROL ( @no_lint ) t nil)
(SK_RULE SK_CONTROL ( progn    ) t
  ;; Check `progn' first argument
  (unless (equal "NO_LINT" (car (SK_ARGS)))
    (foreach map sexp (SK_ARGS) (SK_CHECK_FORM sexp))
    ))

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
              (_\@lint_sexp elts      messages (cons 2 levels) (cons sexp parents) envs            scheme)
              (_\@lint_sexp predicate messages (cons 3 levels) (cons sexp parents) (cons env envs) scheme)
              (@lint_resolve_env sexp messages levels env)
              )
            ))
        nil)
      (@lint_msg sexp messages levels 'ERROR (concat 'SYNTAX_ (upperCase (car sexp)))
        (@str "`{(car sexp)}` syntax should be ({(car sexp)} <variable> <list> <predicate>)"))
      )
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
  (lambda ( sexp messages levels parents envs scheme )
    (cond
      ;; Check minumun number of arguments
      ( (not (cddr sexp)) (@lint_msg sexp messages levels 'ERROR 'CASE_MISSING_ARGS (@str "`{(car sexp)}` requires at least two arguments")) )
      ( t
        (destructuringBind ( fun _val @rest cases ) sexp
          (let ( ( case_sexp_pos 2 )
                 )
            ;; Browse cases
            (foreach case cases
              (cond
                ( (not (listp case))
                  (@lint_msg sexp messages levels 'ERROR 'SYNTAX_CASE (@str "`{fun}` argument should be a list: {case}"))
                  )
                ;; Parse result S-expressions
                ( t
                  (let ( ( sexp_pos 1 )
                         )
                    (foreach sub_sexp (cdr case)
                      (_\@lint_sexp sub_sexp messages
                        (constar sexp_pos++ case_sexp_pos++ levels  )
                        (constar case       sexp             parents)
                        envs scheme
                        )
                      ));foreach ;let
                  ) ;t
                ));cond ;foreach case
            ));let ;dbind
        ));t ;cond 
    ))

;; -------------------------------------------------------
;; let
;; -------------------------------------------------------

(@lint_rule
  ?functions '( let letseq )
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
                     name has_val
                     )
                (foreach def defs
                  def_pos++
                  ;; Make sure variable definition is valid
                  (setq name    nil)
                  (setq has_val nil)
                  (caseq (type def)
                    ( symbol (setq name def) )
                    ( list
                      (@nif (and (symbolp (car def)) (cdr def) (not (cddr def)))
                            ;; Definition is wrong, report it
                        (@lint_msg sexp messages (constar def_pos 1 levels) 'ERROR (concat 'SYNTAX_ (upperCase fun) '_BINDING)
                          (@str "`{fun}` binding must be a symbol or symbol-value pair: {def}"))
                        (setq name     (car def))
                        (setq has_val t        )
                        ;; Check variable definition
                        (_\@lint_sexp (cadr def) messages
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
                  (when (exists env envs env[name])
                    (@lint_msg sexp messages (constar def_pos 1 levels) 'WARNING (concat (upperCase fun) '_SUPERSEDE)
                      (@str "`{fun}` variable {name} is superseded")
                      ))
                  ;; Report two variables having the same name in the same let
                  (cond
                    ( (not name) )
                    ( env[name]
                      (@lint_msg sexp messages (constar def_pos 1 levels) 'WARNING (concat (upperCase fun) '_UNREACHABLE_VAR)
                        (@str "`{fun}` another variable is already called {name}")) )
                    (t (setf env[name] (list nil 'status (if has_val 'assigned 'unused))) )
                    )
                  ));foreach ;let
              (push env envs)
              ;; Check body
              (foreach sub_sexp body
                (_\@lint_sexp sub_sexp messages (cons sexp_pos++ levels) (cons sexp parents) envs scheme)
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
  ?functions '( setq )
  ?control t
  ?rule_fun
  (lambda ( sexp messages levels parents envs scheme )
    (unless
      (errset
        (destructuringBind (fun name value) sexp
          (@if (exists env envs (and (tablep env) env[name]))
               ?var env
               (setf (car envs)[name]->status 'assigned)
            (@lint_msg sexp messages levels 'WARNING 'GLOBAL (@str "`{fun}` global definition: {name}"))
            )
          ;; Check value
          (_\@lint_sexp value messages (cons 2 levels) (cons sexp parents) envs scheme)
          )
        nil)
      (@lint_msg sexp messages levels 'ERROR 'SYNTAX_SETQ (@str "`{(car sexp)}` syntax must be ({(car sexp)} <name> <value>)"))
      )
    ))

;; TODO - (set (quote <name>) <value>)

;; -------------------------------------------------------
;; Functions definition
;; -------------------------------------------------------

(@lint_rule
  ?functions '( define procedure globalProc defun defglobalfun lambda )
  ?control t
  ?rule_fun
  (lambda ( sexp messages levels parents envs scheme )
    (prog ( ( fun  (car sexp) )
            ( args (cdr sexp) )
            ( env  (car envs) )
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
                ))
            ;; Local variable
            ;; In SKILL, `define` can only be used to define global functions
            ( (and scheme (tablep env)) (setf env[name] (list nil 'status 'assigned)) )
            ;; Global variable
            ( t (@lint_msg sexp messages levels 'INFO 'GLOBAL (@str "`{fun}` global definition: {name}")) )
            )
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
              (@str "`{fun}` syntax must be ({fun} ( <name> <args>... ) <body>...)")
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
        ( (memq fun '( defun defglobalfun ))
          (unless (and (symbolp (car  args))
                       (listp   (cadr args))
                       (cddr args)
                       )
            ;; Invalid syntax
            (@lint_msg sexp messages levels 'ERROR (concat 'SYNTAX_ (upperCase fun))
              (@str "`{fun}` syntax must be ({fun} <name> ( <args>... ) <body>...)")
              )
            (return)
            )
          ;; Valid syntax, fetch name, bindings and body
          (setq name     (car  args))
          (setq bindings (cadr args))
          (setq body     (cddr args))
          ;; Define bindings_levels and bindings_pos
          (setq bindings_levels (cons 2 levels))
          (setq bindings_pos    0              )
          (setq body_pos        3              )
          )

        ;; `lambda`
        ( (eq fun 'lambda)
          (unless (and (listp (car args))
                       (cdr args)
                       )
            (@lint_msg sexp messages levels 'ERROR (concat 'SYNTAX_ (upperCase fun))
              (@str "`{fun}` syntax must be ({fun} ( <args>... ) <body>...)")
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

      ;; Define variable
      (cond
        ( (not name)   (assert (eq fun 'lambda) "Only `lambda` should not be defining a variable") )
        ( (tablep env)
          (@nif (memq fun '( globalProc defglobalfun ))
                ;; Definition is local only
                (setf env[name] (list nil 'status 'assigned))
            ;; Definition is local and global
            (setf env[name] (list nil 'status 'global))
            (@lint_msg sexp messages levels 'INFO 'GLOBAL (@str "`{fun}` global definition: {name}"))
            ))
        ( t (@lint_msg sexp messages levels 'INFO 'GLOBAL (@str "`{fun}` global definition: {name}")) )
        );cond

      ;; Make sure bindings syntax is valid, define new_env using bindings
      (let ( ( env         (makeTable t nil)   )
             ( binding_pos (sub1 bindings_pos) )
             name has_val
             )
        (foreach binding bindings
          binding_pos++
          (setq name    nil)
          (setq has_val nil)
          (cond
            ( (memq binding '( @optional @rest @key @aux )) )
            ( (symbolp binding) (setq name binding) )
            ( (and (listp binding) (symbolp (car binding)) (cdr binding) (not (cddr binding)))
              (setq name    (car binding))
              (setq has_val t            )
              ;; Check default value
              (_\@lint_sexp (cadr binding) messages
                (constar 1 binding_pos bindings_levels)
                (constar binding bindings sexp parents)
                (if scheme (cons env envs) envs)
                scheme
                ))
            ( t
              (@lint_msg sexp messages (constar binding_pos bindings_levels) 'ERROR (concat 'SYNTAX_ (upperCase fun) '_BINDING)
                (@str "`{fun}` binding must be a symbol or symbol-value pair: {binding}"))
              ));t ;cond
          (cond
            ( (not name) )
            ( env[name]
              (@lint_msg sexp messages (constar binding_pos bindings_levels) 'WARNING (concat (upperCase fun) '_UNREACHABLE_VAR)
                (@str "`{fun}` another argument is already called {name}")) )
            ( t (setf env[name] (list nil 'status (if has_val 'assigned 'unused))) )
            )
          );foreach
        ;; Check body
        (foreach sub_sexp body
          (_\@lint_sexp sub_sexp messages (cons body_pos++ levels) (cons sexp parents) (cons env envs) scheme)
          );foreach
        ;; Check unused variables
        (@lint_resolve_env sexp messages levels env)
        );let
      );prog
    ))

;; TODO - Rules for `prog`
;; TODO - Report s-expressions after `return` or `go`

;; -------------------------------------------------------
;; Unknown `status` or `sstatus` calls
;; -------------------------------------------------------

;; This is required at least when running Lint from the SKILL Interpreter
;; Otherwise some valid statuses are reported as unknown
(SK_RULE ( status sstatus ) (not (errset (funcall 'status (car (SK_ARGS)))))
  (SK_ERROR UNKNOWN_STATUS_FLAG "Unknown (s)status flag: %N\n" (SK_FORM))
  )

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
        (apply '_\@lint_sexp sub_sexp messages (cons sexp_pos++ levels) (cons sexp parents) args)
        ));foreach ;let
    ))

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
                      (_\@lint_sexp sexp messages (list sexp_pos++) sexps nil scheme)
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

