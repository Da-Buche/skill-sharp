
(@fun advanced_function
  ( ( pos            ?type integer)
    ( opt ?def 12.27 ?type float  )
    )
  ?doc "Return maximum of POS and OPT numbers"
  ?out integer|float
  (max pos opt)
  )

(@fun another_advanced_function
  ( ( pos          ?type integer               )
    @key
    ( key_required ?type symbol                )
    ( key_optional ?type t|nil          ?def t )
    @rest
    ( args         ?type ( string ... )        )
    )
  ?doc "Return all arguments in a list."
  ?out ( integer symbol t|nil string ... )
  (constar pos key_required key_optional args)
  )

