;; ===============================================================================================================
;; Define utilites for Scheme functional programming.
;;
;; A. Buchet - April 2025
;; ===============================================================================================================

(@fun @nil ( @rest _ )
  ?doc    "Always return nil."
  ?out    nil
  ?strict nil
  nil)

(@fun @t ( @rest _ )
  ?doc    "Always return t."
  ?out    t
  ?strict nil
  t)

(@fun @identity
  ( ( e ?type any )
    )
  ?doc "Return input as is."
  ?out any
  e)

(@fun @getter
  ( @rest
    ( props ?type (symbol ...) ?doc "List of nested properties to be fetched." )
    )
  ?doc "Return a function taking one object in argument and return its nested PROPS value."
  ?out funobj
  (lambda (obj) (apply '@get obj props))
  )

;*/

