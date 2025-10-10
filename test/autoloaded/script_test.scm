(@test
  ?fun '@exit
  ?doc "`@exit` can not be tested for obvious reasons..."
  ?skip t

  (@assertion
    (@exit)
    ?out nil
    )

  (@assertion
    (@exit 0)
    ?out nil
    )

  (@assertion
    (@exit 127)
    ?out nil
    )

  )

;; Script description

(@test
  ?fun '@script_get_description
  ?doc "Set and get script description"

  (@assertion
    (@script_set_description "New script description")
    ?out "New script description"
    )

  (@assertion
    (@script_get_description)
    ?out "New script description"
    )

  (@assertion
    (setf (@script_get_description) "Another script description")
    ?out "Another script description"
    )

  (@assertion
    (@script_get_description)
    ?out "Another script description"
    )
  )

(@test ?fun '@script_set_description         ?inherit '@script_get_description)
(@test ?fun 'setf_\@script_get_description   ?inherit '@script_get_description)
(@test ?fun 'setf_\\\@script_get_description ?inherit '@script_get_description)

;; Script version

(@test
  ?fun '@script_get_version
  ?doc "Set and get script version"

  (@assertion
    (@script_set_version "0.0.1")
    ?out "0.0.1"
    )

  (@assertion
    (@script_get_version)
    ?out "0.0.1"
    )

  (@assertion
    (setf (@script_get_version) "1.12.27")
    ?out "1.12.27"
    )

  (@assertion
    (@script_get_version)
    ?out "1.12.27"
    )
  )

(@test ?fun '@script_set_version         ?inherit '@script_get_version)
(@test ?fun 'setf_\@script_get_version   ?inherit '@script_get_version)
(@test ?fun 'setf_\\\@script_get_version ?inherit '@script_get_version)

;; Mock `@script_is_running?'

;(putd '_\@script_is_running? (getd '@script_is_running?))
(putd '@script_is_running? nil)
(@fun @script_is_running? ( @rest _ )
  ?doc "Mocking replacer. Always return t."
  ?out t
  t)

(@test
  ?fun '@script_is_running?
  ?doc "When running SKILL as a script (not inside virtuoso)."

  (@assertion
    (@script_is_running?)
    ?out t
    )
  )

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

(@test
  ?fun '@script_get_arguments
  ?doc "Get script arguments (When running SKILL file directly (outside Virtuoso)."
  ?skip t

  (@assertion
    (tablep (@script_get_arguments))
    ?out t
    )

  (@assertion
    (sort (foreach mapcar dpl (@table_elements (@script_get_arguments)) dpl->name) 'alphalessp)
    ?out '( "args" "command" "help" "optional" "usage" "version" )
    )

  )

(@test
  ?fun '@script_parse_arguments
  ?doc "This is tested in SKILL# metatest."
  ?skip t

  (@assertion
    (@script_parse_arguments)
    ?out nil
    )
  )


