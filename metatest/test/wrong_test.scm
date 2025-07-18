
(@test
  ?fun   'nofun
  ?title "Wrong test"
  ?doc   "Test with an assertion containing both ?out and ?error"

  (@assertion
    (strcat "abc" "def")
    ?out (strcat "abc" "def")
    )

  (@assertion
    (error "Different message than expected.")
    ?out   nil
    ?error "This is a different error message."
    )

  (@assertion
    (concat 'def "ghi")
    ?out (concat 'def "ghi")
    )

  )

