
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

; (progn

;   (@if (getShellEnvVar "IF_VARIABLE")
;        (@info "IF_VARIABLE is defined: {(getShellEnvVar \"IF_VARIABLE\")}")
;     (@warn "IF_VARIABLE is not defined")
;     12)

;   (@nif (getShellEnvVar "NIF_VARIABLE")
;         (@info "NIF_VARIABLE is defined: {(getShellEnvVar \"NIF_VARIABLE\")}")
;     (@warn "NIF_VARIABLE is not defined")
;     27)

;   )

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

  );test


;; TODO - test @while
; ILS-2> l = (list 1 2 3)
; (1 2 3)
; ILS-2> (@while mapc (pop l) ?var toto (println toto))
; (1 2 3)
