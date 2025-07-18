
(@test
  ?fun   'nofun
  ?title "Extra warn message"
  ?doc   "Test with an assertion containing an unexpected warn message."

  (@assertion
    (strcat "abc" "def")
    ?out (strcat "abc" "def")
    )

  (@assertion
    (warn "Different message than expected.")
    ?out  nil
    ?warn "This is a different warn message."
    )

  (@assertion
    (concat 'def "ghi")
    ?out (concat 'def "ghi")
    )

  )

