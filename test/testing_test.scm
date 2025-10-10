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

(@test ?fun '@assertion ?inherit '@test)

