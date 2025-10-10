
(@test
  ?fun '@ciw
  ?doc "This function is simply a shortcut for `hiGetCIWindow`."
  ?skip (not (isCallable 'hiGetCIWindow))

  (@assertion
    (windowp (@ciw))
    ?out t
    )

  )

(@test
  ?fun '@cw
  ?doc "This function is simply a shortcut for `hiGetCurrentWindow`."
  ?skip (not (isCallable 'hiGetCurrentWindow))

  (@assertion
    (when (hiGetCurrentWindow) (windowp (@cw)))
    ?out t
    )

  )

(@test
  ?fun '@ccv
  ?doc "This function is simply a shortcut for `geGetEditCellView`."
  ?skip (not (isCallable 'geGetEditCellView))

  (@assertion
    (when (geGetEditCellView) (dbobjectp (@ccv)))
    ?out t
    )

  )

(@test
  ?fun '@ctf
  ?doc "This function is simply a shortcut for `techGetTechFile`."
  ?skip (not (isCallable 'techGetTechFile))

  (@assertion
    (when (techGetTechFile) (dbobjectp (@ctf)))
    ?out t
    )

  )

