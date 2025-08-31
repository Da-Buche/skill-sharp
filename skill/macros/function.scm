;; ===============================================================================================================
;; Macro to define functions
;;
;; A. Buchet - July 2025
;; ===============================================================================================================

;; `_\@ordinal' is defined here as it is needed in arguments type-check error strings
(defun _\@ordinal ( n )
  "Return N as an ordinal string."
  ;; From 'Ordinal numbers replacement' extracted 27 April 2025
  ;; https://stackoverflow.com/questions/9647202/ordinal-numbers-replacement
  (if (let ( ( r (mod n 100) )
             )
        (and (leqp 11 r)
             (leqp r 13)
             ));and ;let
      ;; eleventh, twelfth, thirteenth
      (lsprintf "%dth" n)
    ;; Every other number
    (lsprintf "%d%s" n (nth (min 4 (mod n 10)) '("th" "st" "nd" "rd" "th")))
    ));if ;fun

(let ()

  ;; Important design choice here
  ;; Arguments are required by default,
  ;; They become optional when they have a ?def value

  (defun build_args_list ( name args )
    "Transforms `@fun' ARGS into a list of arguments understandable by `defun'"
    (let ( ( arg_type 'positional )
           )
      (foreach mapcan arg args
        (cond
          ;; Arg is a list, parse it
          ( (listp arg)
            (assert (symbolp (car arg))
              "@fun - Unsupported argument expression (first element should be a symbol): %N" arg)
            (let ( ( has_def (memq '?def arg) )
                   )
              (if has_def
                  ;; Argument has a default value, it becomes optional
                  (progn
                    (assert (cdr has_def) "@fun - ?def provided without value behind: %N" arg)
                    (caseq arg_type
                      ( positional
                        (setq arg_type 'optional)
                        (list '@optional (list (car arg) (cadr has_def)))
                        )
                      ( t
                        (list (list (car arg) (cadr has_def)))
                        )
                      ));caseq ;progn
                ;; Argument has no default value, it is a required argument
                (caseq arg_type
                  ( positional (list (car arg)) )
                  ( optional   (error "@fun - A positional argument (without ?def) cannot follow an optional one (with ?def): %N" arg) )
                  ( key        (list (list (car arg) `(error ,(lsprintf "%s - ?%s is required" name (car arg))))) )
                  ( rest       (list (car arg)) )
                  ( t (error "Unknown arg_type: %N" arg_type) )
                  );caseq
                )))
          ;; Arg is a keyword
          ( (eq arg '@key)
            (setq arg_type 'key)
             (list arg)
            )
          ( (eq arg '@rest)
            (setq arg_type 'rest)
             (list arg)
             )
          ;; Underscore is tolerated for rest argument
          ( (and (eq arg_type 'rest) (eq arg '_))
            (list '_)
            )
          ;; Arg is not supported
          ( t
            (error "@fun - Unsupported argument expression (it should be a list, @key or @rest): %N" arg)
            )
          ));cond ;foreach mapcan
      ));let ;def

  (defun build_doc ( doc _args )
    "TODO - Parse ARGS to build function docstring"
    doc
    )

  (defun build_args_check ( name args )
    "TODO - Parse args to build type predicates"
    (let ( ( arg_pos  0            )
           ( arg_type '@positional )
           )
      (foreach mapcan arg args
        (cond
          ;; Arg type modifier
          ( (symbolp arg)
            (caseq arg
              ( _ nil )
              ( ( @key @rest ) (setq arg_type arg) nil )
              ( t (error "build_args_check - Unsupported argument, it should be amongst '( @key @rest ): %A" arg) )
              ));caseq ;symbolp
          ;; Regular argument
          ( (listp arg)
            arg_pos++
            (destructuringBind ( arg_name @key ( type '__undefined__ ) @rest _ ) arg
              (unless (eq type '__undefined__)
                (list
                  (caseq arg_type
                    ( @positional
                      `(_\@fun_type_assert ,arg_name ',type
                         ,(lsprintf "%s - %s argument '%s'" name (_\@ordinal arg_pos) arg_name)
                         ))
                    ( @key
                      `(_\@fun_type_assert ,arg_name ',type
                         ,(lsprintf "%s - key argument '?%s'" name arg_name)
                         ))
                    ( @rest
                      (assert (and (listp type) (neq '... (car type)) (eq '... (car (last type))))
                        "@rest argument ?type should be a list that ends with '...: %N" type)
                      `(_\@fun_type_assert ,arg_name ',type
                         ,(lsprintf "%s - rest argument '%s'" name arg_name)
                         ))
                    ( t (error "build_args_check - Unsupported arg_type, it should be amongst '( @positional @key @rest ): %A" arg) )
                    ));caseq ;list
                ));unless ;dbind
            ));listp ;cond
        );foreach mapcan
      ));let ;fun

  (defglobalfun _\@fun ( name args doc global memoize strict out body )
    "`@fun' helper, generates S-expression to define a valid SKILL function."
    (assert doc "@fun - when defining '%N' ?doc is required and should be a string: %N" name doc)
    (let ( ( lambda_sexp
             `( lambda ,(build_args_list name args)
                ,(build_doc doc args)
                ,@(when strict (build_args_check name args))
                    ,@(if (and strict (neq out '__undefined__))
                          `( (_\@fun_type_assert ,(cons 'progn body) ',out ,(lsprintf "%s - output" name)) )
                        body)
                     ) )
           )
      (when memoize (setq lambda_sexp `(@memoize ,lambda_sexp)))
      `( prog1
         (define ,name ,lambda_sexp)
         ,@(when global `( ( when (theEnvironment) ( putd ',name ,name) ) ))
         (setf (@arglist ',name) ',args)
         (setf (@fdoc    ',name) ',doc )
         (setf (@out     ',name) ',out )
         );prog1
       ));let ;fun

  );closure

(let ( ( types_table (makeTable t nil) )
       )

  (defglobalfun @type_add ( symbol predicate "su" )
    "Define a new object type named SYMBOL associated to PREDICATE."
    (assert (not types_table[symbol]) "@type_add - Type '%s' is already defined: %N" symbol types_table[symbol])
    (let ( ( args (arglist predicate) )
           )
      (assert (and args
                   (or (not (cdr args))
                       (stringp (cadr args))
                       (memq (cadr args) '( @optional @key @rest ))
                       ))
        "@type_add - Predicate should have one postional argument: %N %N" predicate (arglist predicate))
      )
    (setf types_table[symbol] predicate)
    )

  (defglobalfun @type? ( symbol object "sg" )
    "Return OBJECT (or t if OBJECT is nil) when its type matches SYMBOL.
Return nil otherwise."
    (when (if (findClass symbol)
              (classp object symbol)
            (funcall (or types_table[symbol] (error "@type? - Unknown type '%s'." symbol)) object)
            )
      (or object t)
      ))

  (@type_add 'any      (lambda ( _obj ) t)                           )
  (@type_add 'callable 'isCallable                                   )
  ;(@type_add 'boolean  'booleanp                                     ) t|nil is more explicit than boolean
  (@type_add 'stdobj   (lambda ( obj ) (classp obj 'standardObject)) )
  (@type_add 'table    'tablep                                       ) ;(lambda (obj) (classp obj 'assocTable))
  (@type_add 'function (lambda ( obj ) (classp obj 'funobj))         )
  (@type_add 'integer  (lambda ( obj ) (classp obj 'fixnum))         )
  (@type_add 'float    (lambda ( obj ) (classp obj 'flonum))         )

  (when (isCallable 'windowp) (@type_add 'window 'windowp))

  (when (isCallable 'dbIsId )
    (@type_add 'db_view
      (lambda ( obj ) (and (dbIsId obj) (equal "cellView" obj->objType)))
      ))

  );closure

(defun _\@fun_type_assert_rec ( obj type_or_class msg )
  "`_\\@fun_type_assert' recursive helper"
  (cond
    ;; nil
    ( (not type_or_class)
      (if obj
          (list nil (lsprintf "%s should be nil: %N" msg obj))
        (list t "")
        ))
    ;; t
    ( (eq t type_or_class)
      (if (eq t obj)
          (list t "")
        (list nil (lsprintf "%s should be t: %N" msg obj))
        ))
    ;; Symbol
    ( (symbolp type_or_class)
      (let ( ( res (errset (@type? type_or_class obj)) )
             )
        (assert res "%s unkown type? '%s'" msg type_or_class)
        (if (car res)
            (list t "")
          (list nil (lsprintf "%s should match class or type '%s': %N" msg type_or_class obj))
          )))
    ;; Atom
    ( (atom type_or_class)
      (error
        "_\\@fun_type_assert_rec - 2nd argument 'type_or_class' can be a symbol or a [nested] list of symbols: %N"
        type_or_class
        ))
    ;; Or list
    ( (eq 'bor (car type_or_class))
      (if (exists sub_type_or_class (cdr type_or_class)
            (car (_\@fun_type_assert_rec obj sub_type_or_class msg)) )
          (list t "")
        (list nil (lsprintf "%s should match class or type '%s': %N" msg (@pretty_print type_or_class) obj))
        ))
    ;; List
    ( t
      (prog ( ( sub_type '__undefined__ )
              ( pos 0                   )
              )
        (unless (listp obj)
          (return (list nil (lsprintf "%s should be a list matching types '%s': %N" msg (@pretty_print type_or_class) obj)))
          )
        (foreach sub_obj obj
          pos++
          (unless type_or_class
            (return (list nil (lsprintf "%s extra element: %N" msg sub_obj)))
            )
          (if (eq '... (car type_or_class))
              (when (eq sub_type '__undefined__)
                (return (list nil (lsprintf "%s type first element cannot be '...'" msg)))
                )
            (setq sub_type (pop type_or_class))
            )
          (let ( ( new_msg (lsprintf "%s %s element" msg (_\@ordinal pos)) )
                 )
            (destructuringBind ( bool res_msg @optional _top_level)
                               (_\@fun_type_assert_rec sub_obj sub_type new_msg)
              ;; As error concerns a list element, we prepare the message to also contain the global list
              (unless bool (return (list nil res_msg t)))
              ));dbind ;let
          );foreach
        (and type_or_class (nequal '(...) type_or_class)
             (return (list nil (lsprintf "%s missing element" msg)))
             );and
        (return (list t ""))
        ))
    ));cond ;fun

(defun _\@fun_type_assert ( obj type_or_class msg )
  "Assert OBJ matches TYPE and return OBJ.
Raise an error otherwise."
  (let ( ( tailrec (status optimizeTailCall) )
         )
    (unwindProtect
      (progn
        (unless (status debugMode) (sstatus optimizeTailCall t))
        ;; Return obj when assertion passed
        (destructuringBind (bool up_msg @optional top_level) (_\@fun_type_assert_rec obj type_or_class msg)
          (if top_level
              (assert bool "%s in %N should match class or type '%s'" up_msg obj (@pretty_print type_or_class))
            (assert bool "%s" up_msg)
            )
          obj
          ));dbind ;progn
      (sstatus optimizeTailCall tailrec)
      );unwindProtect
    ));let ;fun

(@macro @fun ( name
               args
               @key
               doc
               global
               memoize
               ( strict (equal "TRUE" (getShellEnvVar "SKILL_SHARP_STRICT_TYPE_CHECKING")) )
               ( out    '__undefined__                                                     )
               @rest body )
  "TODO - `@fun' implementation is still a draft..."
  (_\@fun name args doc global memoize strict out body))

(@macro @proc ( name_and_args
                @key
                doc
                global
                memoize
                ( strict (equal "TRUE" (getShellEnvVar "SKILL_SHARP_STRICT_TYPE_CHECKING")) )
                ( out    '__undefined__                                                     )
                @rest body )
  "This macro allows C-style writing.
`@proc' is only `@fun' wrapper (like `procedure' for `defun' or `globalProc' for `defglobalfun').
Please refer to `@fun' documentation."
  (_\@fun (car name_and_args) (cdr name_and_args) doc global memoize strict out body)
  )

;*/

