
(@test
  ?fun   'nofun
  ?title "Missing warn message"
  ?doc   "Test with an assertion containing a missing warn message."

  (@assertion
    (strcat "abc" "def")
    ?out (strcat "abc" "def")
    )

  (@assertion
    12+27
    ?out 39
    ?warn "This is a missing warn message."
    )

  (@assertion
    (concat 'def "ghi")
    ?out (concat 'def "ghi")
    )

  )

