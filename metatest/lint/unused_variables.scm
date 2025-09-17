
(progn

  (let ( a b c )

    (setq a 12)

    (println b)

    )

  (let ( ( var        12 )
         ( unused_var 27 )
         )

    (println var)

    )

  (let ( ( var        12 )
         ( unused_var 27 )
         another_unused_var
         )

    (println var)

    )

  (let ()

    (define twelve 12)

    )

  )

