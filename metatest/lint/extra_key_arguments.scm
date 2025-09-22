
(let ()

  (progn
    ?weird 12

    (@if (getShellEnvVar "DUMMY")
      ?var       this
      ?extra_var that
      12
      27
      )

    ?what nil

    )

  ;; This should not be reported
  (progn
    '?do_not_report
    )

  ?unexpected t

  )

