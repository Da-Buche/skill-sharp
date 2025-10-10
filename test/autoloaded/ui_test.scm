
(@test
  ?fun '@get_available_colors
  ?doc "Return the list of available colors from layers defined in DRF."
  ?skip (not (isCallable 'techGetTechFile))

  (@assertion
    (let ( ( colors (@get_available_colors) )
           )
      (and colors (listp colors) (forall color colors (stringp color)))
      )
    ?out t
    )

  )

(@test
  ?fun '@color_icon
  ?doc "Return a colored icon."
  ?skip (not (isCallable 'hiStringToIcon))

  (@assertion
    (hiIsIcon (@color_icon "red"))
    ?out t
    )
  )

(@test
  ?fun '@color_field
  ?doc "Return a color name picking field."
  ?skip (not (isCallable 'hiCreateCyclicField))

  (@assertion
    (eq 'cyclicStruct (type (@color_field ?name 'color_field)))
    ?out t
    )
  )

