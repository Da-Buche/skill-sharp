
(@test
  ?fun   'nofun
  ?title "Extra error message"
  ?doc   "Test with an assertion containing an unexpected error message."

  (@assertion
    (strcat "abc" "def")
    ?out (strcat "abc" "def")
    )

  (@assertion
    (error "Different message than expected.")
    ?error "This is a different error message."
    )

  (@assertion
    (concat 'def "ghi")
    ?out (concat 'def "ghi")
    )

  )

