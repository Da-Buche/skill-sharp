
(@test
  ?fun '_\@fun_type_assert_rec
  ?doc "Make sure type-checking supports symbols, nested lists and '|' operator."

  (@assertion
    (_\@fun_type_assert_rec 12 'fixnum "")
    ?out '( t "" )
    )

  (@assertion
    (_\@fun_type_assert_rec 12.27 'fixnum "")
    ?out '( nil " should match class or type 'fixnum': 12.27" )
    )

  (@assertion
    (_\@fun_type_assert_rec t nil "")
    ?out '( nil " should be nil: t" )
    )

  (@assertion
    (_\@fun_type_assert_rec t t "")
    ?out '( t "" )
    )

  (@assertion
    (_\@fun_type_assert_rec nil t "")
    ?out '( nil " should be t: nil" )
    )

  (@assertion
    (_\@fun_type_assert_rec nil nil "")
    ?out '( t "" )
    )

  ;; Or list

  (@assertion
    (_\@fun_type_assert_rec 12.27 '(fixnum|flonum) "")
    ?out '( t "" )
    )

  (@assertion
    (_\@fun_type_assert_rec nil '(fixnum|flonum) "")
    ?out '( nil " should match class or type 'fixnum|flonum': nil" )
    )

  (@assertion
    (_\@fun_type_assert_rec nil '(fixnum|flonum|list) "")
    ?out '( t "" )
    )

  (@assertion
    (_\@fun_type_assert_rec t '(fixnum|flonum|list) "")
    ?out '( nil " should match class or type 'fixnum|flonum|list': t" )
    )

  ;; List

  (@assertion
    (_\@fun_type_assert_rec '(sym "abc") '(symbol string) "")
    ?out '( t "" )
    )

  (@assertion
    (_\@fun_type_assert_rec '(12 27) '(number string) "")
    ?out '( nil " 2nd element should match class or type 'string': 27" t )
    )

  (@assertion
    (_\@fun_type_assert_rec '(1 2. -3 "" abc) '(number number number string symbol) "")
    ?out '( t "" )
    )

  (@assertion
    (_\@fun_type_assert_rec '(1 2. -3 "" abc) '(number number number string string) "")
    ?out '( nil " 5th element should match class or type 'string': abc" t )
    )

  ;; Infinite list

  (@assertion
    (_\@fun_type_assert_rec '( 12.27 ) '(number ...) "")
    ?out '( t "" )
    )

  (@assertion
    (_\@fun_type_assert_rec '(1 2. -3) '(number ...) "")
    ?out '( t "" )
    )

  (@assertion
    (_\@fun_type_assert_rec '(42 54 "NaN" 12) '(number ...) "")
    ?out '( nil " 3rd element should match class or type 'number': \"NaN\"" t )
    )

  (@assertion
    (_\@fun_type_assert_rec
      '( 12.27 "str" sym abc def ghi jkl )
      '(number string symbol ...)
       "")
    ?out '( t "" )
    )

  (@assertion
    (_\@fun_type_assert_rec '( 12.27 "str" sym 42 ) '(number string symbol ...) "")
    ?out '( nil " 4th element should match class or type 'symbol': 42" t )
    )

  ;; Nested list

  (@assertion
    (_\@fun_type_assert_rec '( 12.27 ("str" sym) 42 ) '(number (string symbol) number) "")
    ?out '( t "" )
    )

  (@assertion
    (_\@fun_type_assert_rec '( 12.27 ("str" sym) 42 ) '(number (symbol string) number) "")
    ?out '( nil " 2nd element 1st element should match class or type 'symbol': \"str\"" t )
    )

  (@assertion
    (_\@fun_type_assert_rec '( 12.27 ("str" sym) ("s0" s1) ) '(number (string symbol) ...) "")
    ?out '( t "" )
    )

  ;; Nested Or

  (@assertion
    (_\@fun_type_assert_rec '( 12 nil (27 "a") abc (42 "b") nil ) '(number nil|(number string)|symbol ...) "")
    ?out '( t "" )
    )

  (@assertion
    (_\@fun_type_assert_rec '( 12 nil (27 "a") abc (42 12) nil ) '(number nil|(number string)|symbol ...) "")
    ?out '( nil " 5th element should match class or type 'nil|(number string)|symbol': (42 12)" t )
    )

  )



(@test
  ?fun '@fun
  ?doc "Define functions and call them."

  ;; Without argument

  (@fun no_args ()
    ?doc "Always return t."
    t
    )

  (@assertion
    (no_args)
    ?out t
    )

  (@assertion
    (no_args 12)
    ?error "no_args: too many arguments (0 expected, 1 given) - (12)"
    )

  (@assertion
    (no_args 12 27)
    ?error "no_args: too many arguments (0 expected, 2 given) - (12 27)"
    )

  ;; Required positional arguments

  (@fun required_positional_args ( (arg0) (arg1) )
    ?doc "Return the two given args in a list."
    (list arg0 arg1)
    )

  (@assertion
    (required_positional_args)
    ?error "required_positional_args: too few arguments (2 expected, 0 given) - nil"
    )

  (@assertion
    (required_positional_args "dummy")
    ?error "required_positional_args: too few arguments (2 expected, 1 given) - (\"dummy\")"
    )

  (@assertion
    (required_positional_args "dummy" "dummy")
    ?out '( "dummy" "dummy" )
    )

  ;; TODO - `@fun' should be deeply tested!

  )

;; =======================================================
;; Errors when defining @fun
;; =======================================================

;; TODO - Those edge cases should be validated in metatest
; (@test
;   ?fun  'nofun
;   ?title "@fun call error"
;   ?doc   "Make sure @fun raises an error without docstring"

;   (@assertion (@fun errorful_def () ())
;     ?error "toto")

;   )

;; =======================================================
;; Tests to guarantee Type-Checking is fully functional
;; =======================================================

;; -------------------------------------------------------
;; string, symbol input/output checks
;; -------------------------------------------------------

(@test
  ?fun  'nofun
  ?title "@fun Simple Input Type-Checking"
  ?doc   "Assertions to guarantee input type-checking works in simple scenarios."

  (@fun string_as_is
    ( ( str ?type string )
      )
    ?doc "Return STR as is."
    ?out string
    ?strict t
    str
    )

  (@assertion
    (string_as_is "12")
    ?out "12"
    )

  (@assertion
    (string_as_is 12)
    ?error "string_as_is - 1st argument 'str' should match class or type 'string': 12"
    )

  );@test


(@test
  ?fun  'nofun
  ?title "@fun Simple Output Type-Checking"
  ?doc   "Assertions to guarantee type-checking works in simple scenarios."

  (@fun string_to_symbol
    ( ( str ?type string )
      )
    ?doc "Return STR as a symbol."
    ;; Output type is wrong on purpose
    ?out string
    ?strict t
    (concat str)
    )

  (@assertion
    (string_to_symbol 27)
    ?error "string_to_symbol - 1st argument 'str' should match class or type 'string': 27"
    )

  (@assertion
    (string_to_symbol "27")
    ?error "string_to_symbol - output should match class or type 'string': \\2\\7"
    )

  );@test


;; -------------------------------------------------------
;; flonum, fixnum, number input/output checks
;; -------------------------------------------------------

(@test
  ?fun  'nofun
  ?title "@fun Simple Numbers Input Type-Checking"
  ?doc   "Assertions to guarantee type-checking works in simple scenarios."


  (@fun errorful_power
    ( ( num ?type flonum )
      ( pow ?type fixnum )
      )
    ?doc "Return NUM raised to power POW."
    ;; Wrong type on purpose
    ?out symbol
    ?strict t
    (expt num pow)
    )

  (@assertion
    (errorful_power 2 2)
    ?error "errorful_power - 1st argument 'num' should match class or type 'flonum': 2"
    )

  (@assertion
    (errorful_power 2. 2.)
    ?error "errorful_power - 2nd argument 'pow' should match class or type 'fixnum': 2.0"
    )

  (@assertion
    (errorful_power 2. 3)
    ?error "errorful_power - output should match class or type 'symbol': 8.0"
    )

  );@test

(@test
  ?fun  'nofun
  ?title "@fun Simple Numbers Input Type-Checking"
  ?doc   "Assertions to guarantee type-checking works in simple scenarios."


  (@fun power
    ( ( num ?type number )
      ( pow ?type number )
      )
    ?doc "Return NUM raised to power POW."
    ?out number
    ?strict t
    (expt num pow)
    )

  (@assertion
    (power 2 2)
    ?out 4
    )

  (@assertion
    (power 3. 2.)
    ?out 9.0
    )

  (@assertion
    (nearlyEqual 9.261 (power 2.1 3))
    ?out t
    )

  )

;; -------------------------------------------------------
;; Check with more arguments and tynes
;; -------------------------------------------------------

(@test
  ?fun  'nofun
  ?title "@fun Positional and Input Type-Checking"
  ?doc   "Assertions to guarantee type-checking works in simple scenarios."

  (@fun reorder
    ( ( required0 ?type number           )
      ( required1 ?type string           )
      ;; TODO - This case should raise an error or be reported by type-checking analysis
      ( optional2 ?type symbol ?def "12" )
      ( optional3 ?type funobj ?def @nil )
      )
    ?doc ""
    ?out any
    ?strict t
    (list optional3 optional2 required1 required0)
    )

  (@assertion
    (reorder 12.27 "test")
    ?error "reorder - 3rd argument 'optional2' should match class or type 'symbol': \"12\""
    )

  (@assertion
    ?skip "any output type is not supported yet..."
    (reorder 12.27 "test" 'abc)
    ?out `( ,@nil 'abc "test" 12.27)
    )

  (@assertion
    (reorder 12.27 "test" 1e1)
    ?error "reorder - 3rd argument 'optional2' should match class or type 'symbol': 10.0"
    )

  );test

(@test
  ?fun '@proc
  ?doc "Define functions and call them."

  ;; Without argument

  @proc( no_args()
    ?doc "Always return t."
    t
    )

  (@assertion
    (no_args)
    ?out t
    )

  (@assertion
    (no_args 12)
    ?error "no_args: too many arguments (0 expected, 1 given) - (12)"
    )

  (@assertion
    (no_args 12 27)
    ?error "no_args: too many arguments (0 expected, 2 given) - (12 27)"
    )

  )

;; -------------------------------------------------------
;; Type checking
;; -------------------------------------------------------

(@test
  ?fun '@type_add
  ?doc "Add a dummy type and test it."

  (@assertion
    (@type? 'list nil)
    ?out t
    )

  (@assertion
    (@type? 'list (list 'a 12.27))
    ?out '(a 12.27)
    )

  (@assertion
    (isCallable
      (@type_add
        'dummy_symbol_float_pair
         (lambda (obj) (and (listp obj) (symbolp (car obj)) (floatp (cadr obj)) (not (cddr obj))))
         ))
    ?out t
    )

  (@assertion
    (@type? 'dummy_symbol_float_pair (list 'a 12.27))
    ?out '(a 12.27)
    )

  )

(@test ?fun '@type? ?inherit '@type_add)

