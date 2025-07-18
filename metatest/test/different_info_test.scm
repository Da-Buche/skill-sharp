
(@test
  ?fun   'nofun
  ?title "Extra info message"
  ?doc   "Test with an assertion containing an unexpected info message."

  (@assertion
    (strcat "abc" "def")
    ?out (strcat "abc" "def")
    )

  (@assertion
    (info "Different message than expected.")
    ?out  nil
    ?info "This is a different info message."
    )

  (@assertion
    (concat 'def "ghi")
    ?out (concat 'def "ghi")
    )

  )

