;; TODO - `@class' should be deeply tested!

(@test
  ?fun '@built_obj
  ?doc "`@built_obj` is meant to make dependent arguments inside a class."

  (@assertion
    (@class
      ?name 'example_class
      ?doc "Example class"
      ( length @arg ?init 1.0 ?type float )
      ( width  @arg ?init 1.0 ?type float )
      ( area        ?init (let ( ( obj (@built_obj) ) ) obj->length*obj->width) ?type integer )
      )
    ?warn "unknown class example_class when defining _ilSharedInitialize method"
    ?out 'example_class
    )

  (@assertion
    (classp (example_class ?length 2.0) 'example_class)
    ?out t
    )

  (@assertion
    (example_class ?length 2.0 ?width 3.0)->area
    ?out 6.0
    )
  )

(@test ?fun '@class ?inherit '@built_obj)

;; Testing this method makes no sense...
(@test ?fun 'initializeInstance ?inherit '@built_obj)

