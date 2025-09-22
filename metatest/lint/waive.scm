;; Waived Lint rules examples


;; Lint should raise INFO CAR_SETOF
(progn 12 (car (setof elt '( ( a b c ) 27 t nil "abc" ) (atom elt))))

;; Lint should not raise INFO CAR_SETOF
(progn "NO_LINT" (car (setof elt '( waived ( a b c ) 27 t nil "abc" ) (atom elt))))
(@no_lint (car (setof elt '( waived ( a b c ) 27 t nil "abc" ) (atom elt))))

(let ()

  (defglobalfun dummy_function0 ()
    "dummy"
    ;; Lint should raise INFO IF_UNLESS & WARNING IF_T
    (if t
        nil
      'then_part
       ))

  ;; Lint should not raise messages here
  (@no_lint
    (defglobalfun dummy_function1 ()
      "dummy"
      (if t
          nil
        'waived_then_part
         ))
    )

  ;; Lint should not raise messages here
  (defun dummy_function2 ()
    "dummy"
    (progn
      "NO_LINT"
      (if t
          nil
        'waived_then_part
         ))
    )
  (dummy_function2)
  )

;; Lint should raise ERROR IF_EXTRA_ARGS
(if (equal "TRUE" (getShellEnvVar "DUMMY_VAR"))
    12
  27
  42
  )

;; Lint should not raise ERROR IF_EXTRA_ARGS
(@no_lint
  (if (equal "TRUE" (getShellEnvVar "DUMMY_VAR"))
      12
    27
    42
    'waived
     ))

(progn
  "NO_LINT"
  (if (equal "TRUE" (getShellEnvVar "DUMMY_VAR"))
      12
    27
    42
    'waived
    ))

;*/

