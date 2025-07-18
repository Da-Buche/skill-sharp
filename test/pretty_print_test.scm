(@test
  ?fun '@pretty_print
  ?doc "Simple examples of pretty printed expressions."

  (@assertion
    (@pretty_print 12)
    ?out "12"
    )

  (@assertion
    (@pretty_print ())
    ?out "nil"
    )

  (@assertion
    (@pretty_print '(progn 12 27))
    ?out "(progn 12 27)"
    )

  (@assertion
    (@pretty_print '(a b c) t)
    ?out "( a b c )"
    )

  (@assertion
    (@pretty_print ''( 12 27 42 ))
    ?out "'( 12 27 42 )"
    )

  )

