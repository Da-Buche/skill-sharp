;; Test to report line numbers inside macros


;; Here we assume that `@foreach_dbind` has no dedicated rule.
;; It might change in the future...

(let ( ( tuples '( ( a ( 12 27 42   ) )
                   ( b ( 1 2 3 4 5  ) )
                   ( c ( 10 9 8 7 6 ) )
                   )
         )
       )

  (foreach tuple tuples
    (println
      (car (setof num (cadr tuple) (geqp 36 num)))
      ))

  (foreach tuple tuples
    (destructuringBind ( _key val ) tuple
      (println
        (car (setof num val (geqp 36 num)))
        )))

  (@foreach_dbind ( _key val ) tuples
    (car (setof num val (geqp 36 num)))
    )

  )

