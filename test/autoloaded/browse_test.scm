
(@test
  ?fun '@list_lcv
  ?doc "Return the list of Library / Cell / Views referenced from a known cell."
  ?skip (not (isCallable 'dbOpenCellViewByType))

  (@assertion
    (@with ( ( cv (dbOpenCellViewByType "rfExamples" "BB_test_bench" "schematic") )
             )
      (@list_lcv ?cellview cv)
      )
    ?out '(("rfLib" "IQ_mod_BB" "symbol")
           ("rfLib" "ind_BB" "symbol")
           ("analogLib" "vsin" "symbol")
           ("rfLib" "PA_PB" "symbol")
           ("analogLib" "cap" "symbol")
           ("rfLib" "cap_BB" "symbol")
           ("analogLib" "ind" "symbol")
           ("analogLib" "res" "symbol")
           ("rfExamples" "BB_test_bench" "schematic")
           ("rfLib" "PA_BB" "symbol")
           ("rfLib" "IQ_modulator" "symbol")
           ("rfLib" "res_BB" "symbol")
           ("analogLib" "port" "symbol")
           ("analogLib" "gnd" "symbol")
           ("analogLib" "gnd" "schematic")
           )
    )
  )



