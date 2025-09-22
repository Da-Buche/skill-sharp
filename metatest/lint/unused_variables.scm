
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

  (let ( assigned_with_set
         ( name 'global_var )
         )

    (set 'assigned_with_set "value")

    ;; Following is here to make sure that (set '<name> ...) and (set <sexp> ...) are treated differently
    (set (car (setof sym (list name) (symbolp sym))) "global_var_value")

    )

  )

