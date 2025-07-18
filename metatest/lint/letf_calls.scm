(let ()

  (@fun call_with_hint
    ( ( str ?type string )
      )
    ?doc    "dummy function"
    ?global t
    (@letf ( ( (rexMagic) t)
             )
      ;; (car (setof ...)) should raise a hint
      (car (setof elt '(1 2 3) (evenp elt)))
      (if (pcreMatchp "([a-z]+)" str)
          (pcreSubstitute "\\1")
        str)
      ));letf ;fun

  (@fun call_without_definitions
    ( ( str ?type string )
      )
    ?doc    "dummy function"
    ?global t
    ;; Empty `@letf' definitions should raise a hint
    (@letf ( )
      (if (pcreMatchp "([a-z]+)" str)
          (pcreSubstitute "\\1")
        str)
      ))

  );closure

