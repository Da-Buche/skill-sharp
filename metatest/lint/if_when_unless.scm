

(let ( var )

  (defglobalfun dummy_fun0 ()
    "dummy"

    ;; IF_EXTRA_ARGS
    (if (getShellEnvVar "DUMMY")
        12
      27
      42
      )

    ;; IF_NIL
    (if (getShellEnvVar "DUMMY")
        nil
      12
      )

    ;; STATIC_CONDITION
    (if t
        12
      27
      )

    (if nil
        12
      27
      )

    (if 12
        27
      42
      )

    (when ""
      12
      27
      42
      )

    (unless var
      '( a b c)
      )

    (unless '( 12 )
      '( a b c)
      )

    );fun

  );closure



