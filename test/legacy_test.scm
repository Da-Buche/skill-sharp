
(@test
  ?fun 'muffleWarnings
  ?doc "Muffle warnings suppresses all warning messages from woport."

  (@assertion
    (warn "printed warning")
    ?warn "printed warning"
    ?out nil
    )

  (@assertion
    (muffleWarnings (warn "hidden warning"))
    ?out nil
    )

  (@assertion
    (progn (muffleWarnings (warn "reported warning")) (getMuffleWarnings))
    ?out '("*WARNING* reported warning")
    )

  )

(@test ?fun 'getMuffleWarnings ?inherit 'muffleWarnings)

(@test
  ?fun '@setf_getShellEnvVar
  ?doc "`@setf_getShellEnvVar` is meant to be used inside `@setf`."

  (@assertion
    (@setf (getShellEnvVar "DUMMY_VARIABLE") "DUMMY_VALUE")
    ?out t
    )

  (@assertion
    (getShellEnvVar "DUMMY_VARIABLE")
    ?out "DUMMY_VALUE"
    )

  (@assertion
    (@setf (getShellEnvVar "DUMMY_VARIABLE") nil)
    ?out t
    )

  (@assertion
    (getShellEnvVar "DUMMY_VARIABLE")
    ?out nil
    )

  (@assertion
    ?doc "It also works on its own."
    (@setf_getShellEnvVar "NEW_VALUE" "DUMMY_VARIABLE")
    ?out t
    )

  (@assertion
    ?doc "It also works on its own."
    (getShellEnvVar "DUMMY_VARIABLE")
    ?out "NEW_VALUE"
    )

  )

