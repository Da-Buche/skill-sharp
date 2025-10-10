(@test
  ?fun '@print_args
  ?doc "Print simple arguments."

  (@assertion
    (@print_args 12+27 42)
    ?info "args: (39 42)"
    ?out '(39 42)
    )
  )

(@test
  ?fun '@print_table
  ?doc "Print simple table contents."

  (@assertion
    (@print_table
      '( ( "Name" "Value" "Description" )
         ( abc    12      just_a_number )
         ( dummy_name dummy_value dummy_description)
         ))
    ?info "        Name        Value        Description\n         abc           12      just_a_number\n  dummy_name  dummy_value  dummy_description"
    ?out t
    )

  (@assertion
    (@print_table
      '( ( "Name" "Value" "Description" )
         ( abc    12      just_a_number )
         ( dummy_name dummy_value dummy_description)
         )
       ?has_headers t)
    ?info "        Name        Value        Description\n                                            \n         abc           12      just_a_number\n  dummy_name  dummy_value  dummy_description"
    ?out t
    )
  )

(@test
  ?fun '@runtime
  ?doc "Runtime prints a nice table."

  (@assertion
    (@runtime 12 27 ?runs 10)
    ?info "S-Expression  User CPU Time (us)  System CPU Time (us)  Clock Time (us)  Page Faults"
    ?out t
    )
  )




