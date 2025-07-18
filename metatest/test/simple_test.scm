;; Simple test

(@test
  ?fun   'nofun
  ?title "Simple Test"
  ?doc   "Dummy Examples"

  (@assertion
    12+27
    ?out 39
    )

  (@assertion
    42-27
    ?out 15
    )

  )

