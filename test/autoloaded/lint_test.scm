

(@test
  ?fun '@lint
  ?doc "Make sure Lint return the expected messages on known files."

  (@assertion
    (@lint ?files (list (@realpath "$SKILL_SHARP_ROOT/metatest/lint/car_setof.scm")))
    ?info "INFO CAR_SETOF at line 5"
    ?out t
    )

  (@assertion
    (@lint ?files (list (@realpath "$SKILL_SHARP_ROOT/metatest/lint/functions_without_docstrings.scm")))
    ?info "INFO GLOBAL at line 1"
    ?warn "WARNING MISSING_DOCSTRING at line 5"
    ?out nil
    )

  )


