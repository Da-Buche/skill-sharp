(defun @alphalessp0 ( str0 str1 "tt" )
  "Improved `alphalessp' for strings containing numbers."
  (negativep (alphaNumCmp str0 str1))
  )

(@test
  ?fun '@alphalessp0
  ?doc "Usage comparison between `alphalessp' and `@alphalessp0'."

  (@assertion
    ?doc "For simple strings `@alphalessp0' behaves like `alphalessp'"
    (@alphalessp0 "a_is_before_b" "b_is_after_a")
    ?out t
    )

  (@assertion
    (alphalessp "a_is_before_b" "b_is_after_a")
    ?out t
    )

  (@assertion
    ?doc "But `@alphalessp0' is smarter with numbers"
    (@alphalessp0 "file_1_a.txt" "file_100_a.txt")
    ?out t
    )

  (@assertion
    (alphalessp "file_1_a.txt" "file_100_a.txt")
    ?out nil
    )

  (@assertion
    ?doc "It works well with `sort'."
    (sort '( "file_1_b.txt" "file_1_a.txt" "file_100_a.txt" "file_12.txt" ) '@alphalessp0)
    ?out  '( "file_1_a.txt" "file_1_b.txt" "file_12.txt" "file_100_a.txt" )
    )

  (@assertion
    (sort '( "file_1_b.txt" "file_1_a.txt" "file_100_a.txt" "file_12.txt" ) 'alphalessp)
    ?out  '( "file_100_a.txt" "file_12.txt" "file_1_a.txt" "file_1_b.txt" )
    )

  )

