; basename "toto"
; ILS-2> (@basename "toto")
; ILS-2> (@basename "~")
; ILS-2> (@basename "/toto")
; ILS-2> (@basename "/abc//toto")
; ILS-2> (@basename "/abc//toto12_27/")
; ILS-2> (@basename "/abc//toto12_27///")
; ▶ basename ~
; ▶ basename '~'
; ▶ basename /'~'
; ▶ basename /'~'/



; ▶ dirname '~'
; ▶ dirname ~
; ▶ dirname /'~'/
; ▶ dirname a
; [root@16cdbc6d30a4 /]# dirname a
; ILS-2> (@dirname "a")
; ILS-2> (@dirname "/a")
; ▶ dirname //
; ▶ dirname ///
; ▶ dirname /
; ▶ dirname /a
; ▶ dirname /a
; ▶ dirname /a/b
; ILS-2> @dirname "//"
; ILS-2> @dirname "."
; ILS-2> @dirname "a"
; ILS-2> @dirname "/a/b"
; ILS-2> @dirname "/a/b/"
; ILS-2> @dirname "/a/"
; ▶ dirname /a/
; ▶ dirname /
; ▶ dirname /a/b
; ▶ dirname a/a/b
; ILS-2> @dirname "a/b/"
; ILS-2> @dirname "a/a/b"
; ILS-2> @dirname "a/a/b"
; ▶ dirname ''
; ILS-2> @dirname "a/a/b"
; ILS-2> @dirname "/a/"



;; TODO - Write utils tests
(@one_newline "")
(@one_newline "This is a very long string over several lines.\nAnother line here\n\n\n...\n\n")


(@test
  ?fun '@ordinal
  ?doc "Test output for simple numbers."

  (@assertion
    (@ordinal 0)
    ?out '"0th"
    )

  (@assertion
    (mapcar '@ordinal (@enumerate 10))
    ?out '( "0th" "1st" "2nd" "3rd" "4th" "5th" "6th" "7th" "8th" "9th" )
    )

  (@assertion
    (mapcar '@ordinal (@enumerate 10 20))
    ?out '( "10th" "11th" "12th" "13th" "14th" "15th" "16th" "17th" "18th" "19th" )
    )

  (@assertion
    (mapcar '@ordinal (@enumerate 40))
    ?out '(  "0th"  "1st"  "2nd"  "3rd"  "4th"  "5th"  "6th"  "7th"  "8th"  "9th"
             "10th" "11th" "12th" "13th" "14th" "15th" "16th" "17th" "18th" "19th"
             "20th" "21st" "22nd" "23rd" "24th" "25th" "26th" "27th" "28th" "29th"
             "30th" "31st" "32nd" "33rd" "34th" "35th" "36th" "37th" "38th" "39th"
             ))

  (@assertion
    (mapcar '@ordinal '( 100 101 9999 ))
    ?out '( "100th" "101st" "9999th" )
    )

  )

(@test
  ?fun '@alphalessp
  ?doc "Usage comparison between `alphalessp' and `@alphalessp'."

  (@assertion
    ?doc "For simple strings `@alphalessp' behaves like `alphalessp'"
    (@alphalessp "a_is_before_b" "b_is_after_a")
    ?out t
    )

  (@assertion
    (alphalessp "a_is_before_b" "b_is_after_a")
    ?out t
    )

  (@assertion
    ?doc "But `@alphalessp' is smarter with numbers"
    (@alphalessp "file_1_a.txt" "file_100_a.txt")
    ?out t
    )

  (@assertion
    (alphalessp "file_1_a.txt" "file_100_a.txt")
    ?out nil
    )

  (@assertion
    ?doc "It works well with `sort'."
    (sort '( "file_1_b.txt" "file_1_a.txt" "file_100_a.txt" "file_12.txt" ) '@alphalessp)
    ?out  '( "file_1_a.txt" "file_1_b.txt" "file_12.txt" "file_100_a.txt" )
    )

  (@assertion
    (sort '( "file_1_b.txt" "file_1_a.txt" "file_100_a.txt" "file_12.txt" ) 'alphalessp)
    ?out  '( "file_100_a.txt" "file_12.txt" "file_1_a.txt" "file_1_b.txt" )
    )

  )

