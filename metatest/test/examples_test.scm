(let ()

  (@test
    ?fun   'nofun
    ?title "Dummy"

    (info "Running assertions - BEGIN\n")

    (@assertion
      ?doc "Catch messages and output in assertion"
      (info "info message\n")
      (warn "warn message\n")
      12+27
      ?info "info message"
      ?warn "warn message"
      ?out 12+42-15
      )

    (@assertion
      ?doc   "Catch messages and error in assertion"
      (info  "info message\n" )
      (warn  "warn message\n" )
      (error "error message\n")
      ?info  "info message"
      ?warn  "warn message"
      ?error "error message"
      )

    (@assertion
      ?doc "Failing assertion"
      (info  "info message\n" )
      (warn  "warn message\n" )
      (error "error message\n")
      12+42
      ?out 12+27
      )

    (info "Running assertions - END\n")
    );@test

  (@test
    ?fun '@rstrip
    ?doc "Examples of using `rstrip'"

    (@assertion
      (@rstrip " Example ")
      ?out " Example"
      )

    (@assertion
      (@rstrip " Example \t ")
      ?out " Example"
      )

    (@assertion
      (@rstrip "Example")
      ?out "Example"
      )

    );@test

  (@test
    ?fun   'nofun
    ?title "Skipped Test"
    ?doc   "This test should be skipped"
    ?skip  t

    (@assertion
      12
      ?out 12
      )
    );@test

  (@test
    ?fun   'nofun
    ?title "Test containing skipped assertion"

    (@assertion
      ?doc "Dummy assertion"
      12+27
      ?out 39
      )

    (@assertion
      ?doc  "Skipped assertion"
      ?skip (or t nil)
      42*2
      ?out 84
      )

    (@assertion
      ?doc  "This should not be skipped assertion"
      ?skip (and t nil)
      42-27
      ?out 15
      )

    )

  );let

