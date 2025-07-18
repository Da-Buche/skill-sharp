;; Make sure test environments are well confined

(@test
  ?fun '@fun
  ?doc "Define functions and call them."

  ;; Without argument

  (@fun no_args ()
    ?doc "Always return t."
    t
    )

  (@assertion
    (no_args)
    ?out t
    )

  (@assertion
    (no_args 12)
    ?error "no_args: too many arguments (0 expected, 1 given) - (12)"
    )

  )

;; Here no_args should not be defined
(@test
  ?fun 'no_args
  ?doc "Define functions and call them."

  (@assertion
    (no_args)
    ?out t
    )

  (@assertion
    (no_args 12)
    ?error "undefined function - no_args"
    )

  )

