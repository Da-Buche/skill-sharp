
(@test
  ?fun     'nofun
  ?title   "This should raise an error as ?skipped is not a valid key argument."
  ?skipped t

  (@assertion
    ?doc "Dummy"
    12
    ?out 12
    )

  )

