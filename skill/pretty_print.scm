;; ===============================================================================================================
;; Pretty print objects
;;
;; A. Buchet - July 2025
;; ===============================================================================================================

(@fun @pretty_print
  ( ( sexp   ?type any            )
    ( quoted ?type t|nil ?def nil )
    )
  ?doc "Pretty print any SEXP."
  ?out string
  (cond
    ;; Any atom, return it as a string
    ( (atom sexp)
      (lsprintf "%N" sexp )
      )
    ;; Quoted list, return it properly
    ( (and (eq 'quote (car sexp))
           (not (cddr sexp))
           )
      (lsprintf "'%s" (@pretty_print (cadr sexp) t))
      )
    ;; `getq' should be replaced by an arrow
    ( (and (eq 'getq (car sexp))
           (symbolp (cadr sexp))
           (symbolp (caddr sexp))
           (not (cdddr sexp))
           )
      (lsprintf "%s->%s" (cadr sexp) (caddr sexp))
      )
    ;; `getqq' should be replaced by a dot
    ( (and (eq 'getqq (car sexp))
           (symbolp (cadr sexp))
           (symbolp (caddr sexp))
           (not (cdddr sexp))
           )
      (lsprintf "%s.%s" (cadr sexp) (caddr sexp))
      )
    ;; `bor' should be replaced by a vertical bar
    ( (and (eq 'bor (car sexp))
           (cdr sexp)
           )
      (buildString (foreach mapcar elt (cdr sexp) (@pretty_print elt quoted)) "|")
      )
    ;; Any other list
    ( t
      (unless (or (eq nil  (car sexp))
                  (symbolp (car sexp))
                  (listp   (car sexp))
                  )
        ;; Maybe setting quoted here is a bit violent...
        (setq quoted t)
        )
      (lsprintf (if quoted "( %s )" "(%s)")
        (buildString (foreach mapcar elt sexp (@pretty_print elt quoted)) " ")
        ))
    ));cond ;fun

;*/

