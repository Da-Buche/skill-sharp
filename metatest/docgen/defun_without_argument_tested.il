
(defun no_args ()
  "This is a simple function without argument.
It always return 12."
  12)

(when (isCallable '@test)
  (@test
    ?fun 'no_args
    ?doc "`no_args' always return 12."

    (@assertion
      (no_args)
      ?out 12
      )

    ))

