
(@test
  ?fun   'nofun
  ?title "Extra error message"
  ?doc   "Test with an assertion containing an unexpected error message."

  (@assertion
    (strcat "abc" "def")
    ?out (strcat "abc" "def")
    )

  (@assertion
    (error "This is an unexpected error message.\n")
    ?out nil
    )

  (@assertion
    (concat 'def "ghi")
    ?out (concat 'def "ghi")
    )

  )

