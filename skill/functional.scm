;; ===============================================================================================================
;; Define utilites for Scheme functional programming.
;;
;; A. Buchet - April 2025
;; ===============================================================================================================

(@fun @getd ( ( name ?type symbol ) )
  ?doc "`getd` wrapper to guarantee output type."
  ?out callable
  (or (getd name) (@error "@getd - Unable to retrieve function named {name}"))
  )

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

(@fun @memoize
  ( ( function ?type funobj )
    )
  ?doc "Return FUNCTION wrapper that caches all the inputs and associated results."
  ?out funobj
  (let ( ( table (makeTable t nil) )
         )
    (lambda ( @rest args ) (car (or table[args] (setf table[args] (list (apply function args))))))
    ))

(let ()

  (@fun @foldl1
    ( ( function ?type callable )
      ( list     ?type list     )
      )
    ?doc "Apply combination FUNCTION to all LIST elements from the left.
See reference : https://en.wikipedia.org/wiki/Fold_(higher-order_function)"
    ?global t
    ;; Make sure tail-call optimization is enabled
    (@letf ( ( (status optimizeTailCall) t )
             )
      (rec function (car list) (cdr list))
      ))

  (@fun rec
    ( ( function ?type callable )
      ( init                    )
      ( list     ?type list     )
      )
    ?doc "Recursive helper for `@foldl1'"
    (@nif list init
      (rec function (funcall function init (car list)) (cdr list))
      ))

  );closure

;; -------------------------------------------------------
;; `hiEnqueueCmd' wrapper
;; -------------------------------------------------------

(let ( ( queue (tconc nil nil) )
       )

  (@fun _\@queue ()
    ?doc "`@queue' helper to be called inside `hiEnqueueCmd'."
    ?out t
    ?global t
    (unless
      (errset
        (while (cdar queue)
          (@if (cddar queue) (funcall (popf (cdar queue)))
            ;; Prevent queue from becoming empty and breaking tconc structure
            (tconc queue nil)
            (assert (not (popf (car queue))) "_\\@queue - A non-nil object was removed from queue...")
            (funcall (popf (car queue)))
            ))
        t)
      ;; Restore queue in case an error occured
      (setq queue (tconc nil nil))
      )
    ;; Always return t
    t)

  (@fun @queue
    ( ( function ?type callable )
      )
    ?doc "Call function at the end of the event queue. This is `hiEnqueueCmd' functional wrapper."
    ?out t
    ?global t
    (tconc queue function)
    (hiEnqueueCmd "(_\\@queue)")
    )

  )

;; -------------------------------------------------------
;; `hiRegTimer' wrapper
;; -------------------------------------------------------

(let ( ( queue_dpl (list nil) )
       ( i         0          )
       )

  (@fun _\@timer
    ( ( id ?type integer )
      )
    ?doc "`@timer' helper to be called inside `hiRegTimer'"
    ?out t
    ?global t
    (funcall (car (remprop queue_dpl id)))
    ;; Always return t
    t)

  (@fun @timer
    ( ( tenths_of_second ?type integer  )
      ( function         ?type callable )
      )
    ?doc "Execute FUNCTION after TENTHS_OF_SECOND. This is `hiRegTimer' functional wrapper."
    ?out t
    ?global t
    ;; `remprop' works with integer but `putprop' does not
    ;; adding dpl prop by hand
    (pushf function (cdr queue_dpl))
    (pushf ++i      (cdr queue_dpl))
    (hiRegTimer (lsprintf "(_\\@timer %d)" i) tenths_of_second)
    )

  );let

;*/

