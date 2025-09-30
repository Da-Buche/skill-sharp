;; ===============================================================================================================
;; Legacy fixes for `@arglist` and `@output`
;;
;; A. Buchet - September 2025
;; ===============================================================================================================

;; =======================================================
;; Fixes arguments and output checks for macros,
;; syntax forms and all missing functions
;; =======================================================

;; TODO - Contact Cadence support about (arglist '...) for all the wrongly defined functions

(setf (@arglist 'if)
  '( ( g_general  ?type general          )
     ( g_general  ?type general          )
     ;; @rest here to support 'then & 'else
     @rest
     ( g_general  ?type general ?def nil )
     ))

(setf (@output 'lambda) 'callable)

;; No idea why but (arglist 'defun) does not mention body
(setf (@arglist 'defun)
  '( ( s_symbol  ?type symbol  )
     ( g_general ?type general )
     ( g_general ?type general )
     @rest
     ( body ?type ( general ... ) )
     ))

(setf (@arglist 'defglobalfun) (@arglist 'defun    ))
(setf (@arglist 'globalProc  ) (@arglist 'procedure))

(setf (@arglist '@macro)
  '( ( name ?type symbol )
     ( args ?type list   )
     ( doc  ?type string )
     @rest
     ( body ?type ( general ...) )
     ))

(setf (@arglist 'defmacro)
  '( ( name ?type symbol )
     ( args ?type list   )
     @rest
     ( body ?type ( general ...) )
     ))

(setf (@arglist 'defclass)
  '( ( s_className ?type symbol )
     ( s_superClassNames ?type ( symbol ... )         ?def nil )
     ( slots             ?type ( symbol general ... ) ?def nil )
     ))

(setf (@arglist 'defmethod) (@arglist 'defun))

(setf (@arglist 'prog)
  '( ( l_list    ?type list    )
     @rest
     ( g_general ?type general )
     ))

(setf (@arglist 'destructuringBind)
  '( ( args ?type list )
     ( list ?type list )
     @rest
     ( body ?type general)
     ))

(setf (@arglist '_destructuringBind) (@arglist 'destructuringBind))

(setf (@arglist 'unwindProtect)
  '( ( body    ?type general )
     ( cleanup ?type general )
     ))

(setf (@arglist 'cfiUnwindProtect) (@arglist 'unwindProtect))


(setf (@arglist 'setf)
  '( ( g_general ?type general )
     ( g_general ?type general )
     ))

;; `setf` helpers
(foreach fun_name '( arrayref car cdr cdar cadr get getd getq getqq getShellEnvVar nth status )
  (setf (@arglist (concat 'setf_ fun_name)) (cons '( obj ?type general ) (@arglist fun_name)))
  )

(setf (@arglist 'push)
  '( ( obj    ?type general )
     ( target ?type list    )
     ))

(setf (@arglist 'pushf) (@arglist 'push))

(setf (@arglist 'funcall)
  '( ( u_function ?type function )
     @rest
     ( g_general ?def nil ?type ( general ... ) )
     ))

(setf (@arglist 'apply)
  '( ( u_function ?type function )
     @rest
     ( g_general ?def nil ?type ( general ... ) )
     ))

;; Printing functions
(foreach function '( info warn error printf lsprintf )
  (setf (@arglist function)
    '( ( string    ?type string  ?def "" )
       @rest
       ( values    ?type ( general ... ) )
       )
     ))

(setf (@arglist 'assert) (cons '( predicate ?type general) (@arglist 'error)))

(setf (@arglist 'for)
  '( ( var ?type symbol  )
     ( beg ?type integer )
     ( end ?type integer )
     @rest
     ( body ?type ( general ...) )
     ))

(setf (@arglist '@while)
  '( ( bool ?type general    )
     @key
     ( var  ?type symbol|nil )
     @rest
     ( body ?type ( general ...) )
     ))

(setf (@arglist '_backquote) '( ( obj ?type general ) ))

(setf (@arglist 'let)
  '( ( bindings ?type list    )
     ( body     ?type general )
     @rest
     ( body     ?type ( general ... ))
     ))

(setf (@arglist 'letseq) (@arglist 'let))

(setf (@arglist 'fprintf)
  '( ( p_port    ?type port   )
     ( t_string  ?type string )
     @rest
     ( g_general ?def nil ?type general)
     ))

(setf (@arglist 'strcat)
  '( ( S_stringSymbol ?type stringSymbol )
     @rest
     ( S_stringSymbol ?type stringSymbol )
     ))

(setf (@arglist 'makeInstance)
  '( ( class ?type class|symbol )
     @rest
     ( args ?type ( general ... ) )
     ))

(setf (@arglist 'nconc)
  '( ( l0 ?type list )
     ( l1 ?type list )
     @rest
     ( ln ?type ( list ... ) )
     ))

(setf (@arglist 'dynamic) '( ( name ?type symbol ) ))

(setf (@arglist 'popf) '( ( place ?type symbol|list ) ))

;*/

