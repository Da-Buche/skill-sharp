;; ===============================================================================================================
;; Testing useful functions
;;
;; A. Buchet - July 2025
;; ===============================================================================================================

;; The following test guarantees that `skill` and `cdsmps` or `virtuoso` tests results are different

(@test
  ?fun 'ipcBeginProcess
  ?doc "Make sure `cdsmps` is working"
  ?skip (not (member (@basename (or (car (getShellArgs)) (getShellEnvVar "SKILL_INTERPRETER") "")) '( "cdsmps" "virtuoso" )))
  (@assertion
    (let ( str ) (ipcWait (ipcBeginProcess "echo 12" "" (lambda (_pid data) (setq str data)))) str)
    ?out "12\n"
    ))

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
      (@sort table[?] ?shape 'printself)
      )
    ?out '( "key" 12 b )
    )

   (@assertion
     ?doc "`@table_elements' returns all the contained values."
     (let ( ( table (makeTable t nil) )
            )
       (setf table[12]    "a")
       (setf table['b]    27 )
       (setf table["key"] 'c )
       (@sort (@table_elements table) ?shape 'printself)
       )
    ?out '( "a" 27 c )
    )

   (@assertion
     ?doc "It works with any table"
     (let ( ( table (makeTable t nil) )
            )
       (for i 12 27
         (setf table[i] i**2)
         )
       (@sort (@table_elements table) ?comp 'lessp)
       )
     ?out '( 144 169 196 225 256 289 324 361 400 441 484 529 576 625 676 729 )
     )
  )


;; =======================================================
;; Strings
;; =======================================================

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

(@test
  ?fun '@exact_replace

  (@assertion
    ?doc "Exact replace works like `pcreReplace'."
    (@exact_replace "name" "This user is called name. name is a nice Name." "John")
    ?out "This user is called John. John is a nice Name."
    )

  (@assertion
    ?doc "Index argument is optional."
    (@exact_replace "name" "This user is called name. name is a nice Name." "John" 2)
    ?out "This user is called name. John is a nice Name."
    )

  (@assertion
    ?doc "Exact replace does not consider special characters like `rexReplace' or `pcreReplace' would."
    (@exact_replace ".*" "Those characters are matching regex input but only this '.*' will be replaced.
Those .* .* will be replaced as well." "_")
    ?out "Those characters are matching regex input but only this '_' will be replaced.
Those _ _ will be replaced as well."
    )
  )

(@test
  ?fun '@repeat_str
  ?doc "`@repeat_str' can be useful to generate exact indentation."

  (@assertion
    (@repeat_str " " 8)
    ?out "        "
    )

  (@assertion
    (@repeat_str "_" 5)
    ?out "_____"
    )

  (@assertion
    ?doc "It works with longer strings."
    (@repeat_str "abc" 3)
    ?out "abcabcabc"
    )

  (@assertion
    ?doc "It also works with negative numbers."
    (@repeat_str "@" -15)
    ?out "@@@@@@@@@@@@@@@"
    )
  )

(@test
  ?fun '@padd

  (@assertion
    ?doc "Padd to the left by default."
    (@padd "name" 15)
    ?out "           name"
    )

  (@assertion
    ?doc "Padd to the right with a negative input."
    (@padd "ThisIsALongerName" -25)
    ?out "ThisIsALongerName        "
    )

  (@assertion
    ?doc "Padd character can be specified."
    (@padd "ThisIsALongerName" -25 "__")
    ?out "ThisIsALongerName________________"
    )

  (@assertion
    ?doc "`@padd' is useful to align elements in a report (knowing the maximum length)."
    (let ( ( pairs   '( ( "name"    "role"     )
                        ( "John"    "manager"  )
                        ( "William" "designer" )
                        ( "Kevin"   "layouter" )
                        ) )
           ( max_len 0 )
           )
      "Calculate maximum name length."
      (foreach pair pairs
        (setq max_len (max max_len (length (car pair))))
        )
      "Print aligned names and roles."
      (foreach pair pairs
        (destructuringBind (name role) pair
          (info "%s %s\n" (@padd name -max_len) role)
          ))
      "Return t"
      t)
    ?out  t
    ?info "\
name    role\n\
John    manager\n\
William designer\n\
Kevin   layouter\n\
"
    )

  )

(@test
  ?fun '@lstrip
  ?doc "`@lstrip' removes left whitespace from input string."

  (@assertion
    (@lstrip "  This string has whitespace before and after.  ")
    ?out "This string has whitespace before and after.  "
    )

  (@assertion
    (@lstrip "\t\tThis string has tabs before and after.\t\t")
    ?out "This string has tabs before and after.\t\t"
    )

  (@assertion
    (@lstrip " \t Same with combination of\twhitespace and tabs.\t \t")
    ?out "Same with combination of\twhitespace and tabs.\t \t"
    )

  )

(@test
  ?fun '@rstrip
  ?doc "`@rstrip' removes right whitespace from input string."

  (@assertion
    (@rstrip "  This string has whitespace before and after.  ")
    ?out "  This string has whitespace before and after."
    )

  (@assertion
    (@rstrip "\t\tThis string has tabs before and after.\t\t")
    ?out "\t\tThis string has tabs before and after."
    )

  (@assertion
    (@rstrip " \t Same with combination of\twhitespace and tabs.\t \t")
    ?out " \t Same with combination of\twhitespace and tabs."
    )

  )

(@test
  ?fun '@strip
  ?doc "`@strip' removes left and right whitespace from input string."

  (@assertion
    (@strip "  This string has whitespace before and after.  ")
    ?out "This string has whitespace before and after."
    )

  (@assertion
    (@strip "\t\tThis string has tabs before and after.\t\t")
    ?out "This string has tabs before and after."
    )

  (@assertion
    (@strip " \t Same with combination of\twhitespace and tabs.\t \t")
    ?out "Same with combination of\twhitespace and tabs."
    )

  (@assertion
    (@strip " \t Combination of\twhitespace and tabs.\nIn a multiline string.  \n\t \t")
    ?out "Combination of\twhitespace and tabs.\nIn a multiline string."
    )

  )

(@test
  ?fun '@one_newline
  ?doc "`@one_newline' makes all strings end with exactly one newline character."

  (@assertion
    (@one_newline "Example message.")
    ?out "Example message.\n"
    )

  (@assertion
    (@one_newline "This is a very long string over several lines.\nAnother line here\n\n\n...\n\n")
    ?out "This is a very long string over several lines.\nAnother line here\n\n\n...\n"
    )

  (@assertion
    (@one_newline "This is a very long string over several lines.\nAnother line here\n\n\n...")
    ?out "This is a very long string over several lines.\nAnother line here\n\n\n...\n"
    )

  (@assertion
    (@one_newline "")
    ?out "\n"
    )

  (@assertion
    (@one_newline "\n")
    ?out "\n"
    )

  (@assertion
    (@one_newline "\n\n\n")
    ?out "\n"
    )

  )




;; =======================================================
;; Numbers
;; =======================================================

(@test
  ?fun '@inf
  ?doc "All numbers are smaller than inf."

  (@assertion
    (lessp 10000000 (@inf))
    ?out t
    )

  (@assertion
    ?doc "All numbers are smaller than inf."
    (greaterp 10000000 (@inf))
    ?out nil
    )

  (@assertion
    ?doc "Incremented inf is still inf."
    (let ( ( inf (@inf) )
           )
      (equal inf+1 inf)
      )
    ?out t
    )

  (@assertion
    ?doc "`@inf' is useful to initialize variables."
    (let ( ( min_length (@inf)                                       )
           ( words      '( "example" "abc" "test" "very_long_word" ) )
           )
      (foreach word words
        (setq min_length (min min_length (length word)))
        )
      min_length
      )
    ?out 3
    )

  )

(@test
  ?fun '@enumerate

  (@assertion
    ?doc "With only one input, enumerates from 0 to n-1."
    (@enumerate 10)
    ?out '( 0 1 2 3 4 5 6 7 8 9 )
    )

  (@assertion
    ?doc "With two input, enumerates from n to m (excluded)."
    (@enumerate 12 27)
    ?out '( 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 )
    )

  (@assertion
    ?doc "Enumerates works with negative step."
    (@enumerate 42.0 40 -0.5)
    ?out '( 42.0 41.5 41.0 40.5 )
    )

  (@assertion
    ?doc "Enumerates works with floats."
    (defun float_lists_equal ( l0 l1 )
      "Return t if all numbers contained in L0 are `nearlyEqual' to respective values in L1, nil otherwise."
      (prog ()
        (foreach val l0
          (unless (nearlyEqual val (pop l1)) (return))
          )
        (if l1 (return)
          (return t)
          )
        ));prog ;def
    (float_lists_equal
      (@enumerate 12 27 1.6)
      '( 12 13.6 15.2 16.8 18.4 20.0 21.6 23.2 24.8 26.4 )
      )
    ?out t
    )

  (@assertion
    ?doc "Step cannot be 0."
    (@enumerate 0 10 0)
    ?error "@enumerate - STEP cannot be 0"
    )

  (@assertion
    ?doc "With only one negative input, enumerates raises an error."
    (@enumerate -10)
    ?error "@enumerate - with positive STEP, BEG has to be less than END: 0 > -10"
    )

  (@assertion
    ?doc "With only one negative input, enumerates raises an error."
    (@enumerate -27 12 -1)
    ?error "@enumerate - with negative STEP, BEG has to be greater than END: -27 < 12"
    )

  )

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


;; =======================================================
;; Miscellaneous
;; =======================================================


;; =======================================================
;; Predicates
;; =======================================================


;; =======================================================
;; Universal getter
;; =======================================================


