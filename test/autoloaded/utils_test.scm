;; ===============================================================================================================
;; Testing useful functions
;; 
;; A. Buchet - July 2025
;; ===============================================================================================================

;; =======================================================
;; Unix utilities
;; =======================================================

(@test
  ?fun '@basename
  ?doc "`@basename' works like Unix `basename`."

  (@assertion
    (@basename "./dir/file")
    ?out "file"
    )

  (@assertion
    (@basename "/dir/file/")
    ?out "file"
    )

  (@assertion
    (@basename "file")
    ?out "file"
    )

  )

(@test
  ?fun '@dirname
  ?doc "`@dirname' works like Unix `dirname`."

  (@assertion
    (@dirname "./dir/file")
    ?out "./dir"
    )

  (@assertion
    (@dirname "/dir/file/")
    ?out "/dir"
    )

  (@assertion
    (@dirname "file")
    ?out "."
    )

  )

(let ( ( paths '( "file"
                  "file/"
                  "/file/"
                  "/dir//file"
                  "/dir/subdir/file"
                  "/dir//subdir///file////"
                  "~"
                  "~/"
                  "'~'"
                  "/'~'"
                  "/'~'/"
                  "/"
                  "///"
                  ) )
       )

  (defun remove_trailing_newline (str)
    "Remove trailing newline in STR if any"
    (pcreReplace (pcreCompile "\n$") str "" 0)
    )

  (@test
    ?fun   'nofun
    ?title "@basename and @dirname"
    ?doc   "Compare `@basename' and `@dirname' to their Unix equivalents."

    (@assertion
      (mapcar '@basename paths)
      ?out (foreach mapcar path paths (remove_trailing_newline (car (@bash (@str "basename \"{path}\"")))))
      )

    (@assertion
      (mapcar '@dirname paths)
      ?out (foreach mapcar path paths (remove_trailing_newline (car (@bash (@str "dirname \"{path}\"")))))
      )
    )

  )

;; =======================================================
;; Lists
;; =======================================================

(@test
  ?fun '@unique
  ?doc "`@unique' works with all kind of atoms."

  (@assertion
    (sort (@unique '( 1 2 3 12 27 42 9 8 7 6 5 4 3 2 1 )) 'lessp)
    ?out '( 1 2 3 4 5 6 7 8 9 12 27 42 )
    )

  (@assertion
    ;; `printself' is required as 'a and "a" are identical regarding `alphaNumCmp'
    (@sort (@unique '( a "a" abc "abc" a b c "a" a ))
      ?shape 'printself
      ?comp  '@alphalessp
      )
    ?out '( "a" "abc" a abc b c )
    )

  (@assertion
    (@sort (@unique '( 12 12.27 27 a "a" abc "abc" a b c "a" a 12 27 ))
      ?shape 'printself
      ?comp  '@alphalessp
      )
    ?out '( "a" "abc" 12 12.27 27 a abc b c )
    )

  )

(@test
  ?fun '@sort
  ?doc "`@sort' solves a common pattern of shaping elements before comparing them."

  (@assertion
    ?doc "This is `sortcar' equivalent."
    (@sort '( ( b 12 ) ( c 42 ) ( a 27 ) ) ?shape 'car ?comp 'alphalessp)
    ?out '( ( a 27 ) ( b 12 ) ( c 42 ) )
    )

  (@assertion
    ?doc "But `sortcadr' does not exist!"
    (@sort '( ( b 12 ) ( c 42 ) ( a 27 ) ) ?shape 'cadr ?comp 'lessp)
    ?out '( ( b 12 ) ( a 27 ) ( c 42 ) )
    )

  (@assertion
    ?doc "By default, `@sort' compares using `@alphalessp'."
    (@sort '( 12 a b 12.27 c "a" 27 "b" 27 "c" ) ?shape 'printself)
    ?out '( "a" "b" "c" 12 12.27 27 27 a b c )
    )

  )

;; =======================================================
;; Tables
;; =======================================================

(@test
  ?fun '@table_elements

  (@assertion
    ?doc "table[?] returns all the defined keys."
    (let ( ( table (makeTable t nil) )
           )
      (setf table[12]    "a")
      (setf table['b]    27 )
      (setf table["key"] 'c )
      table[?]
      )
    ?out '( b "key" 12 )
    )

   (@assertion
     ?doc "`@table_elements' returns all the contained values."
     (let ( ( table (makeTable t nil) )
            )
       (setf table[12]    "a")
       (setf table['b]    27 )
       (setf table["key"] 'c )
       (@table_elements table)
       )
    ?out '( 27 c "a" )
    )

   (@assertion
     ?doc "It works with any table"
     (let ( ( table (makeTable t nil) )
            )
       (for i 12 27
         (setf table[i] i**2)
         )
       (@table_elements table)
       )
     ?out '( 225 529 196 484 169 441 144 400 361 729 324 676 289 625 256 576 )
     )
  )
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

