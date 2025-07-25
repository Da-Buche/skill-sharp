;; ===============================================================================================================
;; Macro to properly apply design patterns
;;
;; A. Buchet - July 2025
;; ===============================================================================================================

;; =======================================================
;; Debugging macro
;; =======================================================

(@fun _\@show
  ( ( quoted_sexp ?type any )
    ( sexp        ?type any )
    )
  ?doc    "Print QUOTED_SEXP and evaluated SEXP, return SEXP."
  ?out    any
  ?strict nil
  (info "%N: %N\n" quoted_sexp sexp)
  sexp
  )

(@macro @show ( @rest sexps )
  "Print each S-expression from SEXPS with evaluated value.
Return the result of last evaluation."
  (if (cdr sexps)
      (cons 'progn (foreach mapcar sexp sexps `(_\@show ',sexp ,sexp)))
    `(_\@show ',(car sexps) ,(car sexps))
    ));if ;macro


;; =======================================================
;; case & caseq
;; =======================================================

(@macro @case ( value @rest cases )
  "`case' wrapper which raises an error if VALUE is not matched to CASES."
  `(case ,value
     ,@cases
     (t (error
          ,(lsprintf "Value is not amongst valid cases (%s): %%N"
             (buildString
               (foreach mapcar sexp cases
                 (lsprintf "%N" (car sexp))
                 )))
           ,value)
       )))

(@macro @caseq ( value @rest cases )
  "`caseq' wrapper which raises an error if VALUE is not matched to CASES."
  `(caseq ,value
     ,@cases
     (t (error
          ,(lsprintf "Value is not amongst valid cases (%s): %%N"
             (buildString
               (foreach mapcar sexp cases
                 (lsprintf "%N" (car sexp))
                 )))
           ,value)
       )))


;; =======================================================
;; wrap
;; =======================================================

(@macro @wrap ( in out @rest body )
  "Generic context manager, run IN, then BODY, and safely execute OUT.

This macro answers to recurring pattern.
Grouping IN and OUT expressions is also often clearer and less error-prone.

See also `@letf' and `@with' for specific context management."
  (assert body "@wrap - BODY cannot be nil: %N" body)
  (list 'unwindProtect (constar 'progn in body) out)
  )


;; =======================================================
;; letf
;; =======================================================

(@macro @setf ( sexp val )
  "`setf' wrapper allowing to supersede protected `setf' helpers.
Behavior is similar except `@setf' will look for a callable `@setf_...' helper before `setf_...' one.

For instance `setf_getShellEnvVar' is wrongly defined. It does not support nil
as a value so it cannot be used with `@letf'.
(When shell variables are not defined, reverting them to nil raise an error and the
temporary value becomes permanent.)

`@letf' uses `@setf' to set and revert values."
  (or (and (listp sexp)
           (symbolp (car sexp))
           (isCallable (concat '@setf_ (car sexp)))
           (constar (concat '@setf_ (car sexp)) val (cdr sexp))
           )
      `(setf ,sexp ,val)
      ))

(@fun _\@letf
  ( ( defs ?type list )
    ( body ?type list )
    )
  ?doc "`@letf' recursive helper."
  ?out list
  (if defs
      (let (def)
        ;; Check defs
        (setq def (car defs))
        (assert (and (listp def) def (car def) (cdr def) (not (cddr def))) "@letf - not a valid pair: %N" def)
        ;; Recursively nest `let' calls containing `unwindProtect' and `setf'
        (list
          `(let ( ( __\@letf_var__ ,(car def) )
                  )
             (unwindProtect
               (progn (setf ,(car def) ,(cadr def)) ,@(_\@letf (cdr defs) body))
               (@setf ,(car def) __\@letf_var__)
               )))
         );let
    ;; No other definition, return body
    body
    ));if ;def

(@macro @letf ( defs @rest body )
  "Use `setf' to redefine values locally.
Any getter call which has an associated `setf_...' setter can be used.
Its value will be set inside BODY and reverted afterwards.

DEFS syntax is similar to `let' definitions except that variables are replaced by calls.
(Like `setf' for `setq')

This is the SKILL++ equivalent to Emacs Lisp `cl-letf'.

See also `@wrap' and `@with' for context management."
  ;; Check unevaluated inputs
  (assert (listp defs) "@letf - DEFS should be a list of pairs: %N" defs)
  (assert defs         "@letf - DEFS cannot be nil: %N"             defs)
  (assert body         "@letf - BODY cannot be nil: %N"             body)
  ;; Return built S-expression
  (car (_\@letf defs body))
  )


;; =======================================================
;; with
;; =======================================================

(@fun _\@with
  ( ( defs ?type list )
    ( body ?type list )
    )
  ?doc "`@with' recursive helper."
  ?out list
  (if defs
      (let ( (def (car defs) )
             )
        ;; Check def
        (assert (and (listp def) def (symbolp (car def)) (cdr def) (not (cddr def))) "@with - not a valid pair: %N" def)
        ;; Recursively nest `let' calls containing `unwindProtect' and `setf'
        (list
          `(let ( ( ,(car def) ,(cadr def) )
                  )
             (unwindProtect
               (progn (@in ,(car def)) ,@(_\@with (cdr defs) body))
               (@out ,(car def))
               )))
         );let
    ;; No other definition, return body
    body
    ));if ;def

(@macro @with ( defs @rest body )
  "Assign DEFS variable-value pairs, like `let' would do, but wrap BODY in a context manager:

1. Call `@in' method for each defined variable.
2. Run BODY S-expressions.
3. Call `@out' method for each defined variable. (This step occurs whatever happended in 1. or 2.)
4. Return final BODY evaluation result.

`@in' and `@out' methods are meant to be redefined for unsupported or custom classes.
They take one positional argument, which is the object being managed.

This is inspired by Python `with` context manager.
(The original idea to do it in SKILL comes from Sebastien Cliquennois [STMicroelectronics]).

See also `@wrap' and `@with' for context management."
  ;; Check unevaluated inputs
  (assert (listp defs) "@with - DEFS should be a list of pairs: %N" defs)
  (assert defs         "@with - DEFS cannot be nil: %N"             defs)
  (assert body         "@with - BODY cannot be nil: %N"             body)
  ;; Return built S-expression
  (car (_\@with defs body))
  )

;; -------------------------------------------------------
;; With context managers
;; -------------------------------------------------------

;; -------------------------------------
;; Ports
;; -------------------------------------

(defmethod @in ( ( _obj port ) @rest _ )
  "Context manager when opening a port, nothing to do..."
  nil)

(defmethod @out ( ( obj port ) @rest _ )
  "Context manager when releasing a port"
  (close obj))


;; -------------------------------------
;; Cellviews
;; -------------------------------------

;; TODO - Cellviews context manager


;; -------------------------------------
;; Property bags
;; -------------------------------------

;; TODO - Property bags context manager





;; =======================================================
;; Anaphoric macros
;;
;; The following macros were inspired by
;; P. Graham's 'On Lisp'
;;
;; Although they are not anaphoric anymore as they require
;; a ?var argument. This is clearer for readers and it
;; also allows to nest the calls.
;; =======================================================

(@macro @if ( @key ( var '__unbound__ ) @rest args )
  "Combination of `let' and `if', to re-use test result inside the 'then' statement.
This a very common pattern.

It also respect Scheme simplifaction where 'else' statement can accept any number of S-expressions.
(There is no need for an extra `progn')"
  (destructuringBind ( test then @rest else ) args
    ;; Shape else
    (setq else
      (cond
        ( (not else      ) nil                       )
        ( (not (cdr else)) (list (car else))         )
        ( t                (list (cons 'progn else)) )
        ));cond ;setq
    ;; Use var when provided
    (cond
      ( (eq '__unbound__ var) `(if ,test ,then ,@else)                                            )
      ( (symbolp         var) `(let ( ( ,var ,test ) ) (if ,var ,then ,@else))                    )
      ( t                     (error "@if ?var should be omitted or an unquotted symbol: %N" var) )
      );cond
    ));dbind ;macro

(@macro @nif ( @key ( var '__unbound__ ) @rest args )
  "Combination of `let' and not `if', to re-use test result inside the 'then' statement.
This a very common pattern.

It also respect Scheme simplifaction where 'then' statement can accept any number of S-expressions.
(There is no need for an extra `progn')"
  (destructuringBind ( test else @rest then ) args
    ;; Shape then
    (setq then
      (cond
        ( (not then      ) nil                       )
        ( (not (cdr then)) (list (car then))         )
        ( t                (list (cons 'progn then)) )
        ));cond ;setq
    ;; Use var when provided
    (cond
      ( (eq '__unbound__ var) `(if ,test ,@then ,else)                                      )
      ( (symbolp         var) `(let ( ( ,var ,test ) ) (if ,var ,@then ,else))             )
      ( t                     (error "@nif ?var should be omitted or an unquotted symbol.") )
      );cond
    ));dbind ;macro

(@macro @when ( @key  (var (error "@when - ?var is required and should be an unquotted symbol.") )
                @rest args "sg" )
  "Combination of `let' and `when', to re-use test result inside the 'then' statement.
This a very common pattern."
  (destructuringBind ( test @rest then ) args
    `(let ( ( ,var ,test )
            )
       (when ,var ,@then)
       )
     ));dbind ;macro

;*/

