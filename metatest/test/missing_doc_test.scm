;; Examples of documented tests except for last one.

(@test
  ?fun   'nofun
  ?title "Documented test"
  ?doc   "Test is valid as it is documented."

  (@assertion
    3**2
    ?out 9
    )

  )


(@test
  ?fun   'nofun
  ?title "Documented test and assertion"
  ?doc   "Test is valid as it is documented."

  (@assertion
    4*5
    ?doc "Documented assertion."
    ?out 20
    )

  ;; This assertion is not documented but this is ok as test is.
  (@assertion
    4*5
    ?out 20
    )

  )


(@test
  ?fun   'nofun
  ?title "Documented assertions"
  ;; Test is valid as all assertions are documented.

  (@assertion
    (strcat "a" "b")
    ?doc "First documented assertion."
    ?out "ab"
    )

  (@assertion
    (strcat "a" "b" "c")
    ?doc "Second documented assertion."
    ?out (strcat "a" "b" "c")
    )

  )


(@test
  ?fun   'nofun
  ?title "Missing assertion documentation"

  (@assertion
    (concat 'a 'b)
    ?doc "Documented assertion."
    ?out 'ab
    )

  ;; This assertion is not documented
  (@assertion
    (concat 'a 'b 'c)
    ?out (concat 'a 'b 'c)
    )

  )

