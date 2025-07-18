
(@test
  ?fun   'nofun
  ?title "Extra info message"
  ?doc   "Test with an assertion containing an unexpected info message."

  (@assertion
    (strcat "abc" "def")
    ?out (strcat "abc" "def")
    )

  (@assertion
    (info "This is an unexpected info message.\n")
    ?out nil
    )

  (@assertion
    (concat 'def "ghi")
    ?out (concat 'def "ghi")
    )

  )

