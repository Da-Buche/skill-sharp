
;; =======================================================
;; Setf helpers
;; =======================================================

(@test
  ?fun 'setf_fdoc
  ?doc "Make sure `(setf (fdoc ...) ...)` and `fdoc` work well."

  (@assertion
    (defglobalfun dummy_function () "example docstring" 12 27)
    ?out 'dummy_function
    )

  (@assertion
    (setf (fdoc 'dummy_function) "updated docstring")
    ?out "updated docstring"
    )

  (@assertion
    (fdoc 'dummy_function)
    ?out "updated docstring"
    )
  )

(@test
  ?fun 'setf_rexMagic
  ?doc "Make sure `(setf (rexMagic) ...)` works."

  (@assertion
    (setf (rexMagic) nil)
    ?out nil
    )

  (@assertion
    (rexMagic)
    ?out nil
    )

  (@assertion
    (setf_rexMagic t)
    (rexMagic)
    ?out t
    )
  )


;; =======================================================
;; Debugging functions
;; =======================================================

(@test
  ?fun '@get_debug
  ?doc "Get & set the debugging status."
  ?skip t

  (@assertion
    (@get_debug)
    ?out nil
    )

  (@assertion
    (@set_debug t)
    ?out t
    )

  (@assertion
    (@get_debug)
    ?out t
    )

  (@assertion
    (setf (@get_debug) nil)
    ?out nil
    )

  (@assertion
    (@get_debug)
    ?out nil
    )
  )

(@test ?fun '@set_debug         ?inherit '@get_debug)
(@test ?fun 'setf_\@get_debug   ?inherit '@get_debug)
(@test ?fun 'setf_\\\@get_debug ?inherit '@get_debug)

(@test
  ?fun '@realpath
  ?doc "Expand variables and symlinks inside a given path."

  (@assertion
    (setShellEnvVar "DUMMY_PATH_EXAMPLE" "/tmp/dir")
    ?out t
    )

  (@assertion
    (@realpath "$DUMMY_PATH_EXAMPLE/subdir/file")
    ?out "/tmp/dir/subdir/file"
    )

  )

;; =======================================================
;; Run Shell Commands
;; =======================================================

(@test
  ?fun '@bash
  ?doc "Make sure `@bash` returns the proper stdout, stdin & exit status values."

  (@assertion
    (@bash "echo simple bash message")
    ?out '("simple bash message\n" "" 0)
    )

  (@assertion
    (@bash ">&2 echo stderr message")
    ?out '("" "stderr message\n" 0)
    )

  (@assertion
    (@bash "echo 12 ; echo 27 >&2 ; exit 3")
    ?out '("12\n" "27\n" 3)
    )

  (@assertion
    (@bash "false")
    ?out '("" "" 1)
    )

  (@assertion
    ?doc "Check stdout, stderr and exit status are well returned."
    (@bash "echo 12 ; >&2 echo 27 ; exit 42")
    ?out '("12\n" "27\n" 42)
    )

  (@assertion
    ?doc "Check special characters are well taken in account"
    (@bash "printf '%-10s\n\"Second line\".\n\n' word")
    ?out '("word      \n\"Second line\".\n\n" "" 0)
    )

  );test

;; =======================================================
;; Loading functions
;; =======================================================

(@test
  ?fun '@load
  ?doc "Make sure `@load` works as expected on a known file."

  (@assertion
    (@load "$SKILL_SHARP_ROOT/metatest/hello_world.scm")
    ?info "Hello World!\n"
    ?out t
    )

  (@assertion
    (@load "$SKILL_SHARP_ROOT/metatest/hello_world.scm" ?no_reload t)
    ?out t
    )

  (@assertion
    (@load "$SKILL_SHARP_ROOT/metatest/hello_world.scm")
    ?info "Hello World!\n"
    ?out t
    )
  )

