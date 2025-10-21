(@test
  ?fun '@globals
  ?doc "Run `@globals` on known files."

  (@assertion
    (@globals ?files (list (@realpath "$SKILL_SHARP_ROOT/metatest/globals/functions.ils")))
    ?out '( ( global_fun nonlocal lambda_variable ) nil nil nil )
    )

  )

(@test
  ?fun '@docgen
  ?doc "Run `@docgen` on known files."

  (@assertion
    (@docgen ?source_files (list (@realpath "$SKILL_SHARP_ROOT/metatest/docgen/defun_without_argument.il")))
    ?out t
    ?info "( \"no_args\"\n  \"no_args()\"\n  \"This is a simple function without argument.\nIt always return 12.\"\n  )"
    )

  )

(@test
  ?fun '@fndcheck
  ?doc "Run `@fndcheck` on known files."

  (@assertion
    (@fndcheck ?files (list (@realpath "$SKILL_SHARP_ROOT/metatest/fndcheck/valid.fnd")))
    ?out t
    )

  (@assertion
    (@fndcheck ?files (list (@realpath "$SKILL_SHARP_ROOT/metatest/fndcheck/warning.fnd")))
    ?warn "Error when reading .fnd file"
    ?out nil
    )

  (@assertion
    (@fndcheck ?files (list (@realpath "$SKILL_SHARP_ROOT/metatest/fndcheck/errorful_characters.fnd")))
    ?warn "character found after backslash is not meaningful"
    ?out t
    )
  )

