;; ===============================================================================================================
;; Useful functions
;;
;; A. Buchet - April 2025
;; ===============================================================================================================

;; =======================================================
;; Unix utilites
;; =======================================================

(@fun @basename
  ( (path ?type string)
    )
  ?doc "Return file name of PATH.
This is Unix `basename` equivalent."
  ?out string
  (if (pcreMatchp "^/+$" path) "/"
    (if (pcreMatchp "/?([^/]+)(/+)?$" path)
        (@letf ( ( (rexMagic) t )
                   )
          (pcreSubstitute "\\1")
          )
      path)));if ;def

(@fun @dirname
  ( (path ?type string)
    )
  ?doc "Return file name of PATH.
This is Unix `dirname` equivalent."
  ?out string
  (if (pcreMatchp "^/+$" path) "/"
    (let ( ( pcre (pcreCompile "/+([^/]+)(/+)?$") )
           )
      (if (pcreMatchp pcre path)
          (let ( ( res (pcreReplace pcre path "" 0) )
                 )
            (if (blankstrp res) "/" res)
            )
        "."));if ;let
    ));if ;def

(defun @mktemp ( @optional (template "") @rest _ "tg" )
  "Unix `mktemp` wrapper.

`makeTempFileName' is limited because it does not support 'XXX...' pattern at the end of TEMPLATE.
It also only generates a file name instead of an actual file. (At least the name is explicit)
This might cause issues if another identical temporary name is generated at the same time.

This is probably equivalent to `mktemp` \"unsafe\" --dry-run mode."
  (destructuringBind (stdout stderr status)
                     (@bash (lsprintf "mktemp %s | xargs printf" template))
    (if (zerop status) stdout (error "@mktemp - %s" stderr))
    ));dbind ;def


;; =======================================================
;; Lists
;; =======================================================

(@fun @unique
  ( ( list ?type list )
    )
  ?doc "Return elements of LIST without duplicates in a non-predictable order."
  ?out list
  (let ( ( table (makeTable t nil) )
         )
    (foreach e list
      (setf table[e] t)
      )
    table[?]
    ));let ;def

(@fun @sort
  ( ( list ?type list )
    @key
    ( shape ?type callable ?def '@identity   ?doc "Shaper function, used on both arguments before comparison. (`@getter' output for instance)" )
    ( comp  ?type callable ?def '@alphalessp ?doc "Comparison function, used to do the sorting."                                               )
    )
  ?doc "Return sorted LIST using COMP as comparison function applied on elements shaped using SHAPE.
(This function is destructive! This is only an enhanced `sort' wrapper.)"
  ?out list
  ;; Make sure functions are properly callable
  (when (symbolp shape) (setq shape (getd shape)))
  (when (symbolp comp ) (setq comp  (getd comp )))
  ;; Return sorted list
  (if (eq (getd '@identity) shape)
      (sort list comp)
    (sort list (lambda (e0 e1) (comp (shape e0) (shape e1))))
    ))


;; =======================================================
;; Tables
;; =======================================================

(@fun @table_elements
  ( ( table ?type table )
    )
  ?doc "Return TABLE elements."
  ?out list
  (foreach mapcar key table[?] table[key])
  )


;; =======================================================
;; Strings
;; =======================================================

(@fun @alphalessp
  ( ( str0 ?type string|symbol )
    ( str1 ?type string|symbol )
    )
  ?doc "Return t if STR0 is lower than STR1 regarding alphanumeric comparison, nil otherwise.

This is an improved `alphalessp' for strings containing numbers, which relies on `alphaNumCmp'.
This comparison works nicely with software versions."
  ?out t|nil
  (negativep (alphaNumCmp str0 str1)))

(@fun @exact_replace
  ( ( pattern     ?type string         )
    ( source      ?type string         )
    ( replacement ?type string         )
    ( index       ?type integer ?def 0 )
    )
  ?doc "Replace all occurences of PATTERN by REPLACEMENT in SOURCE."
  (@letf ( ( (rexMagic) nil )
             )
    (rexCompile pattern)
    (rexReplace source replacement index)
    ));letf ;fun

(@fun @repeat_str
  ( ( str  ?type string  )
    ( n    ?type integer )
    )
  ?doc "Repeat and concatenate STR N times"
  (if (equal " " str) (lsprintf (lsprintf "%%%ds" n) "")
    (@exact_replace " " (lsprintf (lsprintf "%%%ds" n) "") str)
    ))

(@fun @padd
  ( ( str  ?type string           )
    ( num  ?type integer          )
    ( char ?type string  ?def " " )
    )
  ?doc "Padd STR with blankspaces to the left so it contains at least NUM characters.
If NUM is negative, STR is right-padded instead."
  ?out string
  (let ( ( padd_num (difference (abs num) (length str)) )
         )
    (if (plusp padd_num)
        (if (plusp num)
            ;; Left-padd with given character
            (strcat (@exact_replace " " (lsprintf (lsprintf "%%%ds" padd_num) "") char) str)
          ;; Right-padd with given character
          (strcat str (@exact_replace " " (lsprintf (lsprintf "%%%ds" padd_num) "") char))
          );if
      ;; String does not need padding, return it as is
      str
      );if
    ));let ;fun

(@fun @lstrip
  ( ( str ?type string )
    )
  ?doc "Remove leading characters (whitespace by default) from the left side of the string."
  (@letf ( ( (rexMagic) t )
           )
    (pcreReplace (pcreCompile "^\\s*") str "" 0)
    ))

(@fun @rstrip
  ( ( str ?type string )
    )
  ?doc "Remove leading characters (whitespace by default) from the right side of the string."
  (@letf ( ( (rexMagic) t )
           )
    (pcreReplace (pcreCompile "\\s*$") str "" 0)
    ))

(@fun @strip
  ( ( str ?type string )
    )
  ?doc "Remove leading and trailing characters (whitespace by default)."
  (@lstrip (@rstrip str))
  )

;; Alternative solution, it might be a bit faster (depending on regex engine)
;; but not sure if it is safer.
; (@fun @strip
;   ( ( str ?type string )
;     )
;   ?doc "Remove leading and trailing characters (whitespace by default)."
;   (@letf ( ( (rexMagic) t )
;            )
;     ;; (pcreGenCompileOptBits ?setDotAll t) => 4
;     (if (pcreMatchp "^\\s*(.*?)\\s*$" str 4)
;         (pcreSubstitute "\\1")
;       str)
;     ));letf ;fun

(@fun @one_newline
  ( ( str ?type string )
    )
  ?doc "Make sure STR ends with exactly one newline character."
  ?out string
  (pcreReplace (pcreCompile "\n*$") str "\n" 1)
  )

(@fun @escape_chars
  ( ( str ?type string )
    )
  ?doc "Return STR where special characters have been escaped."
  ?out string
  (foreach pair '( ;( "\\" "\\\\" )
                   ( "\"" "\\\"" )
                   )
    (destructuringBind (match replace) pair
      (setq str (@exact_replace match str replace))
      ));destructuringBind ;foreach
  str
  )


;; =======================================================
;; Numbers
;; =======================================================

(@fun @inf ()
  ?doc "Return infinity number (inf)."
  ;; 1e309 is the smallest exonant that works but margin was added for safety
  1e100000
  )

(@fun @enumerate
  ( ( beg  ?type number                               )
    ;; Set default end unless provided
    ( end  ?type number ?def (prog1 beg (setq beg 0)) )
    ( step ?type number ?def 1                        )
    )
  ?doc "Enumerate from BEG to END using STEP.

If END is not provided, END defaults to BEG minus 1 and BEG defaults to 0."
  ?out ( number ... )
  (let ( ( comp (if (plusp step) 'lessp 'greaterp) )
         ( res  (tconc () nil)                     )
         )
    ;; Check input
    (assert (not (zerop step))     "@enumerate - STEP cannot be 0")
    (assert (funcall comp beg end) "@enumerate - with %s STEP, BEG has to be %s than END: %n %s %n"
      (if (plusp step) 'positive 'negative)
      (if (plusp step) 'less     'greater )
      beg
      (if (plusp step) ">"       "<"      )
      end
      )
    (while (funcall comp beg end)
      (tconc res beg)
      (setq beg (plus beg step))
      )
    (cdar res)
    ))

;; This is just a wrapper so `_@ordinal' function documentation is improved
(@fun @ordinal
  ( ( n ?type integer )
    )
  ?doc "Return N as an ordinal string"
  ?out string
  (_\@ordinal n))

(@fun @hex_to_dec
  ( ( hex ?type string )
    )
  ?doc "Convert HEX into decimal."
  ?out integer
  (evalstring (strcat "0x" hex)))

(@fun @dec_to_hex
  ( ( dec    ?type integer )
    ( digits ?type integer ?def 0 ?doc "When digits is positive, it determines the minimum output length.")
    )
  ?doc "Return DEC into hexadecimal."
  ?out string
  (let ( ( res (numConv (lsprintf "%d" dec) "hex" nil) )
         )
    (if (plusp digits)
        (@padd res digits)
      res
      );if
    ));let ;fun


;; =======================================================
;; Miscellaneous
;; =======================================================

(@fun @skill_files
  ( ( paths ?type ( string ... ) )
    )
  ?doc "Return all the SKILL/SKILL++ files found from PATHS."
  ?out ( string ... )
  (foreach mapcan path paths
    (destructuringBind
        (stdout _stderr _status)
        (@bash (@str "find {path} -name '*.scm' -o -name '*.il' -o -name '*.ils'"))
      (parseString stdout "\n")
      )))

(@fun @file_contents
  ( ( path ?type string )
    )
  ?doc "Return the contents of file at PATH as one string."
  ?out string
  ;; Using outstring
  (@with ( ( in  (infile path) )
           ( out (outstring)   )
           )
    (let ( line )
      (while (gets line in) (fprintf out "%s" line))
      )
    (getOutstring out)
    ))


;; =======================================================
;; Predicates
;; =======================================================

(@fun @is?
  ( ( predicate ?type callable )
    ( object    ?type any      )
    )
  ?doc "PREDICATE wrapper.
Return OBJECT when it passes PREDICATE and is non-nil.
Return t when OBJECT passes PREDICATE but is nil.
Return nil otherwise."
  (when (funcall predicate object) (or object t))
  )

(@fun @of_type?
  ( ( type   ?type symbol )
    ( object ?type any    )
    )
  ?doc "`type' predicate wrapper.
Return OBJECT when it is of TYPE and is non-nil.
Return t when OBJECT is of TYPE but is nil.
Return nil otherwise."
  (when (eq type (typep object)) (or object t))
  )

(@fun @of_class?
  ( ( class   ?type symbol )
    ( object ?type any    )
    )
  ?doc "`classp' predicate wrapper.
Return OBJECT when it belongs to CLASS and is non-nil.
Return t when OBJECT belongs to CLASS but is nil.
Return nil otherwise."
  (when (classp object class) (or object t))
  )

(@fun @nonblankstring?
  ( ( obj ?type any )
    )
  ?doc    "Return t if STR is a non-blank string, nil otherwise."
  (and (stringp obj) (not (blankstrp (@strip obj))))
  )

; (@fun @when_list
;   ( ( obj ?type any )
;     )
;   ?doc "If OBJ is non-nil, return it in a list, return nil otherwise.
; This is mostly meant to be used inside `mapcan' or `foreach mapcan'."
;   ?out list
;   (when obj (list obj)))


;; =======================================================
;; Universal getter
;; =======================================================

;; TODO - @get should probably be a method for ( object t )

(@fun @get
  ( ( obj   ?type any          ?doc "Object containing at least one property or even more nested ones." )
    @rest
    ( props ?type (symbol ...) ?doc "List of nested properties to be fetched."                          )
    )
  ?doc "Fetches nested properties PROPS from OBJ."
  ?out any
  (foreach prop props
    (setq obj (get obj prop))
    )
  obj)

(@fun setf_\@get
  ( ( value ?type any          ?doc "Value to be set."                                                  )
    ( obj   ?type any          ?doc "Object containing at least one property or even more nested ones." )
    @rest
    ( props ?type (symbol ...) ?doc "List of nested properties, the last one is to be set."             )
    )
  ?doc "Set last property of properties PROPS to VALUE in OBJ. Return set value."
  ?out any
  (assert props "@set - Please provide at least one property to set.")
  (let ( ( prop (pop props) )
         )
    ;; Fetch all property except last one
    (while props
      (setq obj (get obj prop))
      (setq prop (pop props))
      )
    (setf (get obj prop) value)
    ));let ;fun

;*/

