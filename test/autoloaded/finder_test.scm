(@test
  ?fun '@fnd_gui
  ?doc "Display the finder and return"
  ?skip t

  (@assertion
    (@fnd_gui)
    ?out t

    )
  )

