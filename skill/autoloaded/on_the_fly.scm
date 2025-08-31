;; ===============================================================================================================
;; Useful shortcuts mostly to be used directly from the CIW.
;;
;; A. Buchet - August 2025
;; ===============================================================================================================

(@fun @ciw ()
  ?doc "Return Command Interpreter Window."
  ?out window
  (hiGetCIWindow)
  )

(@fun @cw ()
  ?doc "Return current window"
  ?out window
  (hiGetCurrentWindow)
  )

(@fun @ccv ()
  ?doc "Return current cellview."
  ?out dbobject
  (geGetEditCellView)
  )

(@fun @ctf ()
  ?doc "Return current tech file."
  ?out dbobject
  (techGetTechFile (@ccv))
  )

;*/
