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
;; Booleans
;; =======================================================

(@test
  ?fun '@xor
  ?doc "@xor truth table."

  (@assertion
    (@xor nil nil)
    ?out nil
    )

  (@assertion
    (@xor t nil)
    ?out t
    )

  (@assertion
    (@xor nil t)
    ?out t
    )

  (@assertion
    (@xor t t)
    ?out nil
    )
  )

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

(@test
  ?fun '@mktemp
  ?doc "Temporary files are generated using Unix `mktemp`."

  (@assertion
    (let ( ( file (@mktemp) ) ) (prog1 (and (stringp file) (isFile file)) (deleteFile file)))
    ?out t
    )

  (@assertion
    (let ( ( file0 (@mktemp "file.XXX") )
           ( file1 (@mktemp "file.XXX") )
           )
      (prog1
       (and (stringp file0) (isFile file0)
            (stringp file1) (isFile file1)
            (nequal file0 file1)
            )
       (progn (deleteFile file0) (deleteFile file1))
       ))
    ?out t
    )

  (@assertion
    (@mktemp "test")
    ?error "mktemp: too few X's in template"
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

(@test
  ?fun '@repeat
  ?doc "`@repeat` works with any object."

  (@assertion
    (@repeat "abc" 5)
    ?out '("abc" "abc" "abc" "abc" "abc")
    )

  (@assertion
    (@repeat (list 12 27) 3)
    ?out '((12 27) (12 27) (12 27))
    )

  (@assertion
    ?doc "All elements are identical (each reference is exactly the same pointer)."
    (let ( ( l (@repeat (list 0 1 2 3) 10) ) ) (forall elt (cdr l) (eq elt (car l))))
    ?out t
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

; (@test
;   ?fun '@escape_chars
;   (@assertion
;     (@escape_chars "Escaped characters : \\@ \\\\ \"")
;     ?out "Escaped characters : \\@ \\\\ \\\""
;     )
;   ;; TODO - Not sure @escape_chars is useful
;   )

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

(@test
  ?fun '@hex_to_dec
  ?doc "Return hexadecimal values of given numbers."
  ?skip (not (isCallable 'numConv))

  (@assertion
    (@hex_to_dec "0")
    ?out 0
    )

  (@assertion
    (@hex_to_dec "1")
    ?out 1
    )

  (@assertion
    (@hex_to_dec "A")
    ?out 10
    )

  (@assertion
    (@hex_to_dec "F")
    ?out 15
    )

  (@assertion
    (@hex_to_dec "G")
    ?error "Not a valid hexadecimal number: \"G\""
    )

  (@assertion
    (@hex_to_dec "FF")
    ?out 255
    )

  (@assertion
    (@hex_to_dec "E12AF25")
    ?out 236105509
    )

  (@assertion
    ?doc "Make sure back and forth convertion is valid."
    (forall num '( 0 1 12 27 42 4455 1234567890 )
      (equal num (@hex_to_dec (@dec_to_hex num)))
      )
    ?out t
    )

  )

(@test
  ?fun '@dec_to_hex
  ?doc "Return hexadecimal values of given numbers."
  ?skip (not (isCallable 'numConv))

  (@assertion
    (@dec_to_hex 0)
    ?out "0"
    )

  (@assertion
    (@dec_to_hex 1)
    ?out "1"
    )

  (@assertion
    (@dec_to_hex 10)
    ?out "a"
    )

  (@assertion
    (@dec_to_hex 15)
    ?out "f"
    )

  (@assertion
    (@dec_to_hex 255)
    ?out "ff"
    )

  (@assertion
    (@dec_to_hex 236105509)
    ?out "e12af25"
    )

  (@assertion
    (@dec_to_hex 255 4)
    ?out "00ff"
    )

  (@assertion
    ?doc "Make sure back and forth convertion is valid."
    (forall num '( 0 1 12 27 42 4455 1234567890 )
      (equal num (@dec_to_hex (@hex_to_dec num)))
      )
    ?out t
    )

  )


;; =======================================================
;; Bounding Boxes
;; =======================================================

(@test
  ?fun '@box_width
  ?doc "Check bounding box width for different objects."
  ?skip (not (isCallable 'topEdge))

  (@assertion
    (@box_width (list 0:0 1:1))
    ?out 1
    )

  (@assertion
    (@box_width (list 0.0:12.01 27.3:42.002))
    ?out 27.3
    )

  (@assertion
    (@box_width (list -4.3:-22.123 -0.1:-0.543))
    ?out 4.2
    )

  )

(@test
  ?fun '@box_height
  ?doc "Check bounding box height for different objects."
  ?skip (not (isCallable 'topEdge))

  (@assertion
    (@box_height (list 0:0 1:1))
    ?out 1
    )

  (@assertion
    (@box_height (list 0.0:12.01 27.3:42.002))
    ?out 29.992
    )

  (@assertion
    (@box_height (list -4.3:-22.123 -0.1:-0.543))
    ?out 21.58
    )

  )

;; =======================================================
;; Miscellaneous
;; =======================================================

(@test
  ?fun '@skill_files

  (@assertion
    ?doc "Return SKILL files from a known folder."
    (sort (mapcar '@basename (@skill_files (list "$SKILL_SHARP_ROOT/metatest/globals"))) '@alphalessp)
    ?out '("classes.il" "classes.ils" "definitions.scm" "functions.il" "functions.ils" "variables.il" "variables.ils")
    )
  )

(@test
  ?fun '@read_file
  ?doc "Retrieve contents of known files."

  (@assertion
    (let ( ( tmp_file (@mktemp) )
           )
      (unwindProtect
        (progn
          (@bash (@str "echo 12 > {tmp_file} ; echo 27 >> {tmp_file}"))
          (@read_file tmp_file)
          )
        (deleteFile tmp_file)
        ))
    ?out "12\n27\n"
    )
  )

(@test
  ?fun '@write_file
  ?doc "Retrieve contents of known files."

  (@assertion
    (let ( ( tmp_file (@mktemp) )
           )
      (unwindProtect
        (progn
          (@write_file tmp_file "12\n27\n")
          (@read_file tmp_file)
          )
        (deleteFile tmp_file)
        ))
    ?out "12\n27\n"
    )

  (@assertion
    (let ( ( tmp_file (@mktemp) )
           )
      (unwindProtect
        (progn
          (@write_file tmp_file "12\n27\n")
          (@write_file tmp_file "42\n")
          (@read_file tmp_file)
          )
        (deleteFile tmp_file)
        ))
    ?out "42\n"
    )

  (@assertion
    (let ( ( tmp_file (@mktemp) )
           )
      (unwindProtect
        (progn
          (@write_file tmp_file "12\n27\n")
          (@write_file tmp_file "42\n" "a")
          (@read_file tmp_file)
          )
        (deleteFile tmp_file)
        ))
    ?out "12\n27\n42\n"
    )
  )

;; =======================================================
;; Predicates
;; =======================================================

(@test
  ?fun '@nonblankstring?

  (@assertion
    ?doc "Return nil for anything that is not a string."
    (@nonblankstring? 12)
    ?out nil
    )

  (@assertion
    ?doc "Return nil for an empty string."
    (@nonblankstring? "")
    ?out nil
    )

  (@assertion
    ?doc "Return nil for a whitespace string."
    (@nonblankstring? " \t")
    ?out nil
    )

  (@assertion
    ?doc "Return the provided string otherwise."
    (@nonblankstring? " abc ")
    ?out " abc "
    )

  )

;; =======================================================
;; Universal getter
;; =======================================================

(@test
  ?fun '@get
  ?doc "`@get` can retrieve several properties in one statement."

  (@assertion
    (setq dpl '( nil prop0 12 prop1 ( nil a "a" b "b" c "c" ) prop2 42 ))
    ?out '(nil prop0 12 prop1 (nil a "a" b "b" c "c") prop2 42)
    )

  (@assertion
    (@get dpl 'prop1 'a)
    ?out "a"
    )

  (@assertion
    (setf (@get dpl 'prop1 'a) 27)
    ?out 27
    )

  (@assertion
    (@get dpl 'prop1 'a)
    ?out 27
    )
  )

(@test ?fun 'setf_\@get ?inherit '@get)

;; =======================================================
;; Dbobjects
;; =======================================================

(unless (findClass 'dbobject)
  (defclass dbobject ()
    ( ( libName      @initarg libName      @initform nil )
      ( cellName     @initarg cellName     @initform nil )
      ( viewName     @initarg viewName     @initform nil )
      ( cellViewType @initarg cellViewType @initform nil )
      )
    ))

(defvar test_db_cv
  (makeInstance 'dbobject
    ?libName      "TEST_LIB"
    ?cellName     "TEST_CELL"
    ?viewName     "schematic"
    ?cellViewtype "schematic"
    ))

(@test
  ?fun '@lcv
  ?doc "Return the library, cell and view of input dbobject."

  (@assertion
    (@lcv test_db_cv)
    ?out '( "TEST_LIB" "TEST_CELL" "schematic")
    )
  )

(@test
  ?fun '@view_type
  ?doc "Return the view type whatever the input."
  ?skip (not (isCallable 'ddGetObj))

  (@assertion
    (@view_type test_db_cv)
    ?out "schematic"
    )

  ;; TODO - test with ddview, list and string as well
  ;; TODO - Build read-only & writable test libraries to run when running tests inside Virtuoso
  )

;; =======================================================
;; Tech Files
;; =======================================================

(@test
  ?fun '@tech_libs
  ?doc "Return the current tech libraries."
  ?skip (not (isCallable 'techGetTechFile))

  (@assertion
    (and (@tech_libs) (forall lib (@tech_libs) (ddIsId lib)))
    ?out t
    )

  (@assertion
    (car (member "analogLib" (@tech_libs)~>name))
    ?out "analogLib"
    )

  )

(@test
  ?fun '@tech_files
  ?doc "Return the current tech libraries."
  ?skip (not (isCallable 'techGetTechFile))

  (@assertion
    (and (@tech_files) (forall tf (@tech_files) (dbobjectp tf)))
    ?out t
    )

  )

;; =======================================================
;; Windows
;; =======================================================

(@test
  ?fun '@window_number
  ?doc "Return the window number of any window or session window."
  ?skip (not (isCallable 'windowp))

  (@assertion
    (@window_number (@ciw))
    ?out 1
    )

  ;; TODO - Test for classic windows
  ;; TODO - Test for session windows
  )

;; =======================================================
;; Menus
;; =======================================================

(@test
  ?fun '@menu_by_label
  ?doc "Return menu by label"
  ?skip (not (isCallable 'hiGetBannerMenus))

  (@assertion
    (hiIsMenu (@menu_by_label ?window (@ciw) ?label "tools"))
    ?out t
    )
  )

(@test
  ?fun '@menu_item_by_label
  ?doc "Return menu item by label"
  ?skip (not (isCallable 'hiGetBannerMenus))

  (@assertion
    (type (@menu_item_by_label ?window (@ciw) ?menu_label "tools" ?label "SKILL API Finder"))
    ?out 'hiMenuItem
    )
  )

(@test
  ?fun '@menu_replace_item
  ?skip t

  (@assertion
    ?doc "Replace SKILL API Finder"
    (@menu_replace_item
      ?window            (hiGetCIWindow)
      ?menu_label        "Tools"
      ?item_label        "SKILL API Finder"
      ?new_item_icon     (hiLoadIconData (@realpath "$SKILL_SHARP_ROOT/pictures/icons/sharp.png"))
      ?new_item_callback "(if (equal \"TRUE\" (getShellEnvVar \"SKILL_SHARP_KEEP_NATIVE_FINDER\")) (startFinder) (@fnd_gui))"
      )
    ?out t
    )

  )

(@test
  ?fun '@menu_insert_item_before
  ?skip t

  (@assertion
    ?doc "Add SKILL# API Finder"
    (@menu_insert_item_before
      ?window            (hiGetCIWindow)
      ?menu_label        "Tools"
      ?item_label        "SKILL API Finder"
      ?new_item_name     'skill_sharp_api_finder_item
      ?new_item_label    "SKILL# API Finder"
      ?new_item_icon     (hiLoadIconData (@realpath "$SKILL_SHARP_ROOT/pictures/icons/sharp.png"))
      ?new_item_callback "(if (equal \"TRUE\" (getShellEnvVar \"SKILL_SHARP_KEEP_NATIVE_FINDER\")) (startFinder) (@fnd_gui))"
      )
    ?out t
    )

  )

