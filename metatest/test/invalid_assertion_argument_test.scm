
(@test
  ?fun   'nofun
  ?title "This should raise an error as ?skipped is not a valid key argument."

  (@assertion
    ?doc     "Dummy"
    ?skipped t
    12
    ?out 12
    )

  )

