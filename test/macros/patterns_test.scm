
;; =======================================================
;; Debugging macro
;; =======================================================

(@test
  ?fun '@show

  (@assertion
    ?doc "Using `@show' with one argument"

    (@show 12+27)
    ?info "(plus 12 27): 39"
    ?out  39
    )

  (@assertion
    ?doc "With several inputs, only the last expression is returned."
    (@show (rexMagic) (rexMatchp "12" "This contains 12!") "12")
    ?info "(rexMagic): t\n(rexMatchp \"12\" \"This contains 12!\"): t\n\"12\": \"12\""
    ?out "12"
    )

  )

;; =======================================================
;; case & caseq
;; =======================================================

(@test
  ?fun '@case
  ?doc "Works like `case` but raises meaningful errors in unsupported cases."

  (@assertion
    (@case 12
      ( 12 'even )
      ( 27 'odd  )
      )
    ?out 'even
    )

  (@assertion
    (@case "ellipse"
      ( "rect"    'rectangle )
      ( "ellipse" 'ellipse   )
      ( "polygon" 'polygon   )
      )
    ?out 'ellipse
    )

  (@assertion
    (@case "inst"
      ( "rect"    'rectangle )
      ( "ellipse" 'ellipse   )
      ( "polygon" 'polygon   )
      )
    ?error "Value is not amongst valid cases (\"rect\" \"ellipse\" \"polygon\")"
    )
  )

(@test
  ?fun '@caseq
  ?doc "Works like `caseq` but raises meaningful errors in unsupported cases."

  (@assertion
    (@caseq 12
      ( 12 'even )
      ( 27 'odd  )
      )
    ?out 'even
    )

  (@assertion
    (@caseq 'ellipse
      ( rect    'rectangle )
      ( ellipse 'ellipse   )
      ( polygon 'polygon   )
      )
    ?out 'ellipse
    )

  (@assertion
    (@caseq 'inst
      ( rect    'rectangle )
      ( ellipse 'ellipse   )
      ( polygon 'polygon   )
      )
    ?error "Value is not amongst valid cases (rect ellipse polygon)"
    )
  )

;; =======================================================
;; wrap
;; =======================================================

(@test
  ?fun '@wrap

  (@assertion
    ?doc "@wrap executes operation in the right order."
    (@wrap (println 'BEG) (println 'END) (println 'BODY) 12)
    ?out 12
    ?info "BEG\nBODY\nEND\n"
    )

  (@assertion
    ?doc "@wrap executes end expression even when an error occurs."
    (@wrap (println 'BEG) (println 'END) (error "BODY") 12)
    ?info "BEG\nEND\n"
    ?error "BODY"
    )
  )

;; =======================================================
;; letf
;; =======================================================

(@test
  ?fun '@letf

  (@assertion
    ?doc "Set `rexMagic' to t locally"
    (@letf ( ( (rexMagic) t)
               )
      (when (pcreMatchp "(ab)(.+)" "abc")
        (pcreSubstitute "\\0 \\1 \\2")
        ))
    ?out "abc ab c"
    )

  (@assertion
    ?doc "Set `rexMagic' to nil locally"
    (@letf ( ( (rexMagic) nil)
               )
      (when (pcreMatchp "(ab)(.+)" "abc")
        (pcreSubstitute "\\0 \\1 \\2")
        ))
    ?out "\\0 \\1 \\2"
    )

  );test

;; =======================================================
;; with
;; =======================================================

(@test
  ?fun '@with

  (@assertion
    ?doc "`@with' properly closes ports"
    (letseq ( ( port (outstring) )
              ( str  (@with ( ( out_port port )
                              )
                       (fprintf out_port "Hello World!\n")
                       (getOutstring out_port)
                       ))
              )
      (list (openportp port) str)
      )
    ?out (list nil "Hello World!\n")
    )

  ;; TODO - test @with with dummy cellview using ?skip

  );test

;; =======================================================
;; Anaphoric macros
;; =======================================================

(@test
  ?fun '@if

  (@assertion
    ?doc "`@if` works like `if` when condition is non-nil."
    (@if t 'then 'else 12)
    ?out 'then
    )

  (@assertion
    ?doc "`@if` else statement can contain several expressions."
    (@if nil 'then 'else 12)
    ?out 12
    )

  (@assertion
    ?doc "`@if` can store the condition result into a variable."
    (@if 12+27
      ?var res
      (list res res)
      'else
       )
    ?out '(39 39)
    )

  )

(@test
  ?fun '@nif

  (@assertion
    ?doc "`@nif` works like not `if` when condition is nil."
    (@nif nil 'else 'then 12)
    ?out 'else
    )

  (@assertion
    ?doc "`@nif` then statement can contain several expressions."
    (@nif t 'else 'then 12)
    ?out 12
    )

  (@assertion
    ?doc "`@nif` can store the condition result into a variable."
    (@nif 12+27
      ?var res
      'then
      (list res res)
      )
    ?out '(39 39)
    )

  )

(@test
  ?fun '@when

  (@assertion
    ?doc "`@when` works like `when` without variable."
    (eval '(@when t 'then 12))
    ?error "?var is required and should be an unquoted symbol"
    )

  (@assertion
    ?doc "`@when` can store the condition result into a variable."
    (@when 12+27
      ?var res
      'then
      (list res res)
      )
    ?out '(39 39)
    )

  )

;; =======================================================
;; While
;; =======================================================

(@test
  ?fun '@while

  (@assertion
    ?doc "`@while` returns the list of last loop values by default."
    (let ( ( i 3 )
           )
      (@while (plusp i) i--)
      )
    ?out '(3 2 1)
    )

  (@assertion
    ?doc "`@while mapc` returns the list condition results."
    (let ( ( l (list 1 2 3) )
           )
      (@while mapc (pop l))
      )
    ?out '(1 2 3)
    )

  (@assertion
    ?doc "`@while mapcar` returns the list of last loop values."
    (let ( ( i 3 )
           )
      (@while mapcar (plusp i) i--)
      )
    ?out '(3 2 1)
    )

  (@assertion
    ?doc "`@while mapcan` returns the concatenated list of last loop values."
    (let ( ( i 3 )
           )
      (@while mapcan (plusp i) (list i-- t))
      )
    ?out '(3 t 2 t 1 t)
    )

  ;; Using ?var

  (@assertion
    ?doc "`@while` accepts ?var and returns the list of last loop values by default."
    (let ( ( l (list 1 2 3) )
           )
      (@while (pop l)
        ?var elt
        (println elt)
        (list elt elt)
        )
      )
    ?info "1\n2\n3\n"
    ?out '((1 1) (2 2) (3 3))
    )

  (@assertion
    ?doc "`@while mapc` accepts ?var and returns the list condition results."
    (let ( ( l (list 1 2 3) )
           )
      (@while mapc (pop l)
        ?var elt
        (println elt)
        )
      )
    ?info "1\n2\n3\n"
    ?out '(1 2 3)
    )

  (@assertion
    ?doc "`@while mapcar` accepts ?var and returns the list of last loop values."
    (let ( ( l (list 1 2 3) )
           )
      (@while mapcar (pop l)
        ?var elt
        (println elt)
        (list elt elt)
        )
      )
    ?info "1\n2\n3\n"
    ?out '((1 1) (2 2) (3 3))
    )

  (@assertion
    ?doc "`@while mapcan` accepts ?var and returns the concatenated list of last loop values."
    (let ( ( l (list 1 2 3) )
           )
      (@while mapcan (pop l)
        ?var elt
        (println elt)
        (list elt elt)
        )
      )
    ?info "1\n2\n3\n"
    ?out '(1 1 2 2 3 3)
    )

  )

;; =======================================================
;; For
;; =======================================================

(@test
  ?fun '@for

  (@assertion
    ?doc "`@for` returns the list of last loop values."
    (@for i 0 9 i)
    ?out '(0 1 2 3 4 5 6 7 8 9)
    )

  (@assertion
    ?doc "`@for mapcar` returns the list of last loop values."
    (@for mapcar var 0 2 (println var) (list var var))
    ?info "0\n1\n2\n"
    ?out '((0 0) (1 1) (2 2))
    )

  (@assertion
    ?doc "`@for mapcan` returns the concatenated list of last loop values."
    (@for mapcan var 0 2 (println var) (list var var))
    ?info "0\n1\n2\n"
    ?out '(0 0 1 1 2 2)
    )

  )

;; =======================================================
;; Foreach D-bind
;; =======================================================

(@test
  ?fun '@foreach_dbind

  (@assertion
    ?doc "`@foreach_dbind` returns the list of last loop values."
    (@foreach_dbind ( key value ) '( ( a 12) ( b 27 ) ( c 42 ) )
      (println key)
      (list value value)
      )
    ?info "a\nb\nc\n"
    ?out '((12 12) (27 27) (42 42))
    )

  (@assertion
    ?doc "`@foreach_dbind mapc` returns the list of inputs."
    (@foreach_dbind mapc ( key value ) '( ( a 12) ( b 27 ) ( c 42 ) )
      (println key)
      (list value value)
      )
    ?info "a\nb\nc\n"
    ?out '((a 12) (b 27) (c 42))
    )

  (@assertion
    ?doc "`@foreach_dbind` returns the list of last loop values."
    (@foreach_dbind mapcar ( key value ) '( ( a 12) ( b 27 ) ( c 42 ) )
      (println key)
      (list value value)
      )
    ?info "a\nb\nc\n"
    ?out '((12 12) (27 27) (42 42))
    )

  (@assertion
    ?doc "`@foreach_dbind` returns the list of last loop values."
    (@foreach_dbind mapcan ( key value ) '( ( a 12) ( b 27 ) ( c 42 ) )
      (println key)
      (list value value)
      )
    ?info "a\nb\nc\n"
    ?out '(12 12 27 27 42 42)
    )

  )

