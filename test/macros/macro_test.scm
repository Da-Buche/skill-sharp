;; -------------------------------------------------------
;; @arglist
;; -------------------------------------------------------

(@test
  ?fun '_\@arglist_expand
  ?doc "`_\\@arglist_expand' is further tested in `@arglist`."

  (@assertion
    (_\@arglist_expand '( arg0 arg1 ))
    ?out '((arg0) (arg1))
    )

  (@assertion
    (_\@arglist_expand '( arg0 arg1 "tg" ))
    ?out '((arg0 ?type string) (arg1 ?type general))
    )

  (@assertion
    (_\@arglist_expand '( arg0 arg1 @key key_arg @rest argn "tgsg" ))
    ?out '((arg0 ?type string) (arg1 ?type general) @key (key_arg ?def nil ?type symbol) @rest (argn ?def nil ?type general))
    )

  (@assertion
    (_\@arglist_expand '( arg0 arg1 @optional opt_arg @rest argn "tgsg" ))
    ?out '((arg0 ?type string) (arg1 ?type general) (opt_arg ?def nil ?type symbol) @rest (argn ?def nil ?type general))
    )

  )

(@test
  ?fun '@arglist
  ?doc "Retrieves argument of some well known functions."

  (@assertion
    (@arglist '@arglist)
    ?out '((fun ?type function))
    )

  (@assertion
    (@arglist '@alphalessp)
    ?out '((str0 ?type (string | symbol)) (str1 ?type (string | symbol)))
    )

  (@assertion
    (@arglist (lambda ( arg0 arg1 @key key_arg @rest argn "tgsg" ) nil))
    ?out '((arg0 ?type string) (arg1 ?type general) \@key (key_arg ?def nil ?type symbol) \@rest (argn ?def nil ?type general))
    )

  )

(@test
  ?fun 'setf_\@arglist
  ?doc "`@arglist` output can be modified on demand."

  (@assertion
    (inSkill (defmacro dummy_macro (@rest args) args))
    ?out 'dummy_macro
    )

  (@assertion
    (@arglist 'dummy_macro)
    ?out '((___))
    )

  (@assertion
    (setf (@arglist 'dummy_macro) '(\@rest (args ?def nil)))
    ?out '(@rest (args ?def nil))
    )

  (@assertion
    (@arglist 'dummy_macro)
    ?out '(@rest (args ?def nil))
    )

  )

;; -------------------------------------------------------
;; @fdoc
;; -------------------------------------------------------

(@test
  ?fun '@fdoc
  ?doc "Return docstrings of well-known functions."

  (@assertion
    (@fdoc '@alphalessp)
    ?out "Return t if STR0 is lower than STR1 regarding alphanumeric comparison, nil otherwise.\n\nThis is an improved `alphalessp' for strings containing numbers, which relies on `alphaNumCmp'.\nThis comparison works nicely with software versions."
    )

  (@assertion
    (defglobalfun dummy_function () "Dummy docstring" 12 27)
    ?out 'dummy_function
    )

  (@assertion
    (@fdoc 'dummy_function)
    ?out "Dummy docstring"
    )

  (@assertion
    (setf (@fdoc 'dummy_function) "Docstring added afterwards.")
    ?out "Docstring added afterwards."
    )

  (@assertion
    (@fdoc 'dummy_function)
    ?out "Docstring added afterwards."
    )

  )

(@test
  ?fun     'setf_\@fdoc
  ?inherit '@fdoc
  )

;; -------------------------------------------------------
;; @output
;; -------------------------------------------------------

(@test
  ?fun '@output
  ?doc "Return output of well-known functions."

  (@assertion
    (@output '@alphalessp)
    ?out '(t | nil)
    )

  (@assertion
    (defglobalfun dummy_function () t)
    ?out 'dummy_function
    )

  (@assertion
    (@output 'dummy_function)
    ?out nil
    )

  (@assertion
    (setf (@output 'dummy_function) t)
    ?out t
    )

  (@assertion
    (@output 'dummy_function)
    ?out t
    )
  )

(@test
  ?fun     'setf_\@output
  ?inherit '@output
  )

;; -------------------------------------------------------
;; @macro
;; -------------------------------------------------------

(@test
  ?fun '@macro
  ?doc "`@macro` defines macro and stores valid docstring."

  (@assertion
    (inSkill (@macro dummy_wrap ( in out @rest body ) "macro docstring" (list 'unwindProtect (constar 'progn in body) out)))
    ?out 'dummy_wrap
    )

  (@assertion
    (expandMacro '(dummy_wrap in out body))
    ?out '(unwindProtect (progn in body) out)
    )

  (@assertion
    (@fdoc 'dummy_wrap)
    ?out "macro docstring"
    )

  )

