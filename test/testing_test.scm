;; Mock @test behavior so that documentation is well printed.

(defmethod @get_assertions ( (test list) )
  "Get TEST assertions objects."
  test->assertions
  )

(setf @test.@test
  `(nil
     doc        "@test is used to define tests. A test should only contain assertion calls."
     assertions
     ( ( nil
         body_quoted
         (@test
           ?fun '@xor
           ?doc "`@xor` is SKILL boolean XOR equivalent."
           (@assertion
             (@xor t t)
             ?out nil
             )
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
           )
         body_result nil
         )
       )
     ))

;; Testing functions are already tested in metatest as they cannot test themselve properly...
(@test ?fun '@assertion         ?inherit '@test)
(@test ?fun '@get_assertions    ?inherit '@test)
(@test ?fun '@set_status        ?inherit '@test)
(@test ?fun '@update_status     ?inherit '@test)
(@test ?fun '@test_print_report ?inherit '@test)
(@test ?fun '@test_run_all      ?inherit '@test)


(@test
  ?fun 'printself

  (@assertion
    ?doc "`printself` works properly on integers"
    (printself 12)
    ?out "12"
    )

  (@assertion
    ?doc "`printself` works properly on strings"
    (printself "abc")
    ?out "\"abc\""
    )

  (@assertion
    ?doc "`printself` works properly on lists"
    (printself (list 12 27 ""))
    ?out "(12 27 \"\")"
    )
  )


