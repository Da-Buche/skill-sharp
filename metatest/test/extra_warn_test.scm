
(@test
  ?fun   'nofun
  ?title "Extra warn message"
  ?doc   "Test with an assertion containing an unexpected warn message."

  (@assertion
    (strcat "abc" "def")
    ?out (strcat "abc" "def")
    )

  (@assertion
    (warn "This is an unexpected warn message.\n")
    ?out nil
    )

  (@assertion
    (concat 'def "ghi")
    ?out (concat 'def "ghi")
    )

  )

