
(@test
  ?fun '@getd
  ?doc "`@getd` returns the same results as `getd`."

  (@assertion
    (isCallable (@getd 'println))
    ?out t
    )

  (@assertion
    (@getd 'println)
    ?out (getd 'println)
    )

  (@assertion
    (@getd '@getd)
    ?out (getd '@getd)
    )

  (@assertion
    ?doc "`@getd` raises an error when function does not exist."
    (@getd 'this_function_does_not_exist)
    ?error "Unable to retrieve function named this_function_does_not_exist"
    )

  )

(@test
  ?fun '@nil
  ?doc "Always return nil."

  (@assertion
    (@nil)
    ?out nil
    )

  (@assertion
    (@nil 12 27)
    ?out nil
    )

  (@assertion
    (@nil t)
    ?out nil
    )

  )

(@test
  ?fun '@t
  ?doc "Always return t."

  (@assertion
    (@t)
    ?out t
    )

  (@assertion
    (@t 12 27)
    ?out t
    )

  (@assertion
    (@t nil)
    ?out t
    )

  )

(@test
  ?fun '@identity
  ?doc "Always return input argument as is."

  (@assertion
    (@identity t)
    ?out t
    )

  (@assertion
    (@identity (list 12 27))
    ?out '( 12 27 )
    )

  (@assertion
    (@identity 12 27)
    ?error "@identity: too many arguments"
    )

  )

(@test
  ?fun '@getter
  ?doc "Return a getter function"

  (@assertion
    (isCallable (@getter 'prop0))
    ?out t
    )

  (@assertion
    (funcall (@getter 'prop1) '( nil prop0 12 prop1 27 prop2 42 ))
    ?out 27
    )

  (@assertion
    (funcall (@getter 'prop1 'a) '( nil prop0 12 prop1 ( nil a "a" b "b" c "c" ) prop2 42 ))
    ?out "a"
    )
  )

(@test
  ?fun '@memoize

  (@assertion
    ?doc "Define `memoize_test`."
    (getFunType (putd 'memoize_test (@memoize (lambda (obj) (println 'this_will_be_printed_only_once) (times obj obj)))))
    ?out 'lambda
    )

  (@assertion
    ?doc "First time the function is called with a given set of arguments, body is processed and symbol is printed."
    (memoize_test 12)
    ?info "this_will_be_printed_only_once"
    ?out 144
    )

  (@assertion
    ?doc "Any other time the function is called with a known set of arguments, output value is directly return and body is not processed."
    (memoize_test 12)
    ?out 144
    )

  )

(@test
  ?fun '@foldl1
  ?doc "Foldl works with simple functions."

  (@assertion
    (@foldl1 'plus '( 0 1 2 3 4 ))
    ?out 10
    )

  (@assertion
    (@foldl1 'difference '( 1 2 3 4 5 ))
    ?out -13
    )

  (@assertion
    (@foldl1 'times '( 1 2 3 4 5 6 ))
    ?out 720
    )

  (@assertion
    (@foldl1 '@xor '( t t t nil nil t t nil ))
    ?out t
    )

  )

(@test
  ?fun '@queue
  ?doc "@queue cannot be tested using the standard framework."
  ?skip t
  ;; TODO - test `@queue` inside shellspec script

  (@assertion
    (@queue (lambda () (println 'this_is_queued)))
    ?out t
    )

  )

(@test
  ?fun '@timer
  ?doc "@timer cannot be tested using the standard framework."
  ?skip t

  (@assertion
    (@timer 10 (lambda () (println 'this_is_delayed)))
    ?out t
    )

  (@assertion
    (ipcSleep 2)
    ?info "this_is_delayed"
    ?out t
    )

  )

