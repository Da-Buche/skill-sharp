(let ()

  (defun local_fun () "Return t." t)

  (@test
    ?fun 'local_fun
    ?doc "Dummy test."
    (@assertion (local_fun) ?out t)
    )

  (defglobalfun global_fun () "Return t." t)

  (@test
    ?fun 'global_fun
    ?doc "Dummy test."
    (@assertion (global_fun) ?out t)
    )

  );closure

;; This definition and test are OK as they are local.
(let ()

  (defun local_fun () "Return nil." nil)

  (@test
    ?fun 'local_fun
    ?doc "Another local_fun test."
    (@assertion (local_fun) ?out nil)
    )

  )

;; A global function should not be tested twice.
(@test
  ?fun 'global_fun
  ?doc "Dummy test."
  (@assertion (global_fun) ?out t)
  )

