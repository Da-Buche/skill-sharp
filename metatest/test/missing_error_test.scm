
(@test
  ?fun   'nofun
  ?title "Missing error message"
  ?doc   "Test with an assertion containing a missing error message."

  (@assertion
    (strcat "abc" "def")
    ?out (strcat "abc" "def")
    )

  (@assertion
    12+27
    ?error "This is a missing error message."
    )

  (@assertion
    (concat 'def "ghi")
    ?out (concat 'def "ghi")
    )

  )

