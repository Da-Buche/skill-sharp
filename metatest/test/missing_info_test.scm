
(@test
  ?fun   'nofun
  ?title "Missing info message"
  ?doc   "Test with an assertion containing a missing info message."

  (@assertion
    (strcat "abc" "def")
    ?out (strcat "abc" "def")
    )

  (@assertion
    12+27
    ?out 39
    ?info "This is a missing info message."
    )

  (@assertion
    (concat 'def "ghi")
    ?out (concat 'def "ghi")
    )

  )

