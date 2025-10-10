
(@test
  ?fun '@to_string

  (@assertion
    ?doc "Works with symbols."
    (@to_string 'abc)
    ?out "abc"
    )

  (@assertion
    ?doc "Works with symbols containing special characters."
    (@to_string '@abc_\@\ def\-0)
    ?out "@abc_@ def-0"
    )

  (@assertion
    ?doc "Works with strings"
    (@to_string "this is a string")
    ?out "this is a string"
    )

  (@assertion
    ?doc "Works with strings and spec"
    (@to_string "this is a string" "%N")
    ?out "\"this is a string\""
    )

  (@assertion
    ?doc "Works with atoms."
    (mapcar '@to_string '( 12.27 42 nil))
    ?out '( "12.27" "42" "nil" )
    )

  (@assertion
    ?doc "Works with lists."
    (@letf ( ( @str.pretty nil ) ) (@to_string '( 12.27 42 nil '( a b c ))))
    ?out "(12.27 42 nil (quote (a b c)))"
    )

  (@assertion
    ?doc "Works with lists (pretty printed)."
    (@letf ( ( @str.pretty t ) ) (@to_string '( 12.27 42 nil '( a b c))))
    ?out "( 12.27 42 nil '( a b c ) )"
    )

  )

(@test
  ?fun '@str

  (@assertion
    ?doc "Simple replacement."
    (@str "The result of 12+27 is {12+27}.")
    ?out "The result of 12+27 is 39."
    )

  (@assertion
    ?doc "Variables replacement."
    (let ( ( a 12 ) ( b 27 ) ) (@str "The result of {a}+{b} is {a+b}."))
    ?out "The result of 12+27 is 39."
    )

  (@assertion
    ?doc "Double-brackets are not evaluated."
    (let ( ( var 'evaluated ) ) (@str "This is {var}, this is not {{evaluated}}."))
    ?out "This is evaluated, this is not {evaluated}."
    )

  (@assertion
    ?doc "Formatting is taken in account."
    (let ((str "simple string")) (@str "no format: {str}, %s format: {str%s}, %N format: {str%N}"))
    ?out "no format: simple string, %s format: simple string, %N format: \"simple string\""
    )

  (@assertion
    ?doc "Non-closed brackets raise errors."
    (eval '(let ( ( var 'evaluated ) ) (@str "This is {var")))
    ?error "Open-bracket is never closed in f-string"
    )

  (@assertion
    ?doc "Non-open brackets are [maybe] okay?"
    (let ( ( var 'evaluated ) ) (@str "This is var}"))
    ?out "This is var}"
    )

  )

(@test
  ?fun '@debug
  ?doc "Messages are printed only when using debugging mode."

  (@assertion
    (@letf ( ( (@get_debug) nil ) ) (@debug "This should not be printed"))
    ?out nil
    )

  (@assertion
    (@letf ( ( (@get_debug) t ) ) (@debug "This should be printed"))
    ?out nil
    ?info "This should be printed"
    )

  )

(@test
  ?fun '@info
  ?doc "Info messages using f-string formatting."

  (@assertion
    (@info "The result of 12+27 is {12+27}")
    ?out nil
    ?info "The result of 12+27 is 39"
    )

  (@assertion
    (@info "The value of pi is {(acos -1)%7.4f}")
    ?out nil
    ?info "The value of pi is  3.1416"
    )

  (@assertion
    (let ( ( str "this is a string" ) ) (@info "Single-quotes : '{str}' ; Double-quotes : {str%N}"))
    ?out nil
    ?info "Single-quotes : 'this is a string' ; Double-quotes : \"this is a string\""
    )
  )

(@test
  ?fun '@warn
  ?doc "Warn messages using f-string formatting."

  (@assertion
    (@warn "The result of 12+27 is {12+27}")
    ?out nil
    ?warn "The result of 12+27 is 39"
    )

  (@assertion
    (@warn "The value of pi is {(acos -1)%7.4f}")
    ?out nil
    ?warn "The value of pi is  3.1416"
    )

  (@assertion
    (let ( ( str "this is a string" ) ) (@warn "Single-quotes : '{str}' ; Double-quotes : {str%N}"))
    ?out nil
    ?warn "Single-quotes : 'this is a string' ; Double-quotes : \"this is a string\""
    )
  )

(@test
  ?fun '@error
  ?doc "Error messages using f-string formatting."

  (@assertion
    (@error "The result of 12+27 is {12+27}")
    ?error "The result of 12+27 is 39"
    )

  (@assertion
    (@error "The value of pi is {(acos -1)%7.4f}")
    ?error "The value of pi is  3.1416"
    )

  (@assertion
    (let ( ( str "this is a string" ) ) (@error "Single-quotes : '{str}' ; Double-quotes : {str%N}"))
    ?error "Single-quotes : 'this is a string' ; Double-quotes : \"this is a string\""
    )
  )

(@test
  ?fun '@fprintf
  ?doc "Fprintf messages using f-string formatting."

  (@assertion
    (@with ( ( port (outstring) ) ) (@fprintf port "The result of 12+27 is {12+27}") (getOutstring port))
    ?out "The result of 12+27 is 39"
    )

  (@assertion
    (@with ( ( port (outstring) ) ) (@fprintf port "The value of pi is {(acos -1)%7.4f}") (getOutstring port))
    ?out "The value of pi is  3.1416"
    )

  (@assertion
    (@with ( ( port (outstring) ) )
      (let ( ( str "this is a string" ) )
        (@fprintf port "Single-quotes : '{str}' ; Double-quotes : {str%N}")
        (getOutstring port)
        ))
    ?out "Single-quotes : 'this is a string' ; Double-quotes : \"this is a string\""
    )
  )

(@test
  ?fun '@assert
  ?doc "Assertions with error message using f-string formatting"

  (@assertion
    (@assert 12+27 "This message will never be printed.")
    ?out nil
    )

  (@assertion
    (let ( ( val 12) ) (@assert (oddp val) "Value should be odd: {val}"))
    ?error "Value should be odd: 12"
    )

  (@assertion
    (let ( ( val "12.27") ) (@assert (numberp val) "Value should be a number: {val}"))
    ?error "Value should be a number: 12.27"
    )

  (@assertion
    ?doc "Message is clearer when using %N formatting."
    (let ( ( val "12.27") ) (@assert (numberp val) "Value should be a number: {val%N}"))
    ?error "Value should be a number: \"12.27\""
    )

  )

