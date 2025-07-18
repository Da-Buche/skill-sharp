;; Waived Lint rules examples


;; This should raise a Lint INFO VAR16
(let ( ( reported_var 12 ) )
  (let ( ( reported_var 27 ) )
    (list reported_var reported_var)
    ))

;; This should be OK
(@no_lint
  (let ( ( ignored_var 12 ) )
    (let ( ( ignored_var 27 ) )
      (list ignored_var ignored_var)
      ))
  )

;; This should raise a Lint hint
(progn (if t nil 'then_part) )

;; This should not
(progn "NO_LINT" (if t nil 'waived_then_part))

;*/

