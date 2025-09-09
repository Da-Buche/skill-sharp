;; ===============================================================================================================
;; Fully custom Lint as native one was too buggy and not suitable for SKILL++ and SKILL#.
;; 
;; A. Buchet - September 2025
;; ===============================================================================================================

;; =======================================================
;; Lint waiver
;; =======================================================

(@macro @no_lint ( @rest body )
  "Lint waiver, equivalent to `progn'."
  (constar 'progn "NO_LINT" body))


(let ()

  (defun lint_rec ()
    "Recursive helper."
    )

  (@fun @lint
    ( @key
      ( files ?type ( string ... )                )
      ( port  ?type port           ?def (@poport) )
      )
    ?doc "Run Sharper Lint on FILES.
All report messages are printed to PORT."

    )

);closure

;*/

