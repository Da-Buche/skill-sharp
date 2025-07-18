;; Mock `@script_is_running?'

(putd '@script_is_running? nil)
(@fun @script_is_running? ( @rest _ )
  ?doc "Mocking replacer. Always return t."
  ?out t
  t)

(@test
  ?fun '@script_add_argument

  (@assertion
    ?doc "Fail with ?callback and ?action"
    (@script_add_argument
      ?keys     '("--verbose")
      ?doc      "Enhance verbosity."
      ?action   'store_true
      ?callback (lambda _ (println 12))
      )
    ?error "@script_add_argument - Error when defining argument \"verbose\".\n\
Both ?callback and ?action cannot be provided, please remove one argument."
    )

  (@assertion
    ?doc "Positional argument can not be defined after optional one"
    ;; Optional argument
    (@script_add_argument
      ?name    "optional"
      ?doc     "optional argument"
      ?default ""
      )
    (@script_add_argument
      ?name "required"
      ?doc  "required argument"
      )
    ?error "@script_add_argument - Error when defining argument \"required\".\n\
A positional required argument (i.e. without ?default) cannot be defined after an optional one."
    )

  ;; TODO - Wrong ?action error should print the list of valid actions.
  ;; TODO - Wrong ?type   error should print the list of valid types.
  ;; TODO - Wrong ?type, ?callback, ?action combination errors should be explicit.

  )


