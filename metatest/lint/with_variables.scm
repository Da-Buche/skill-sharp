(let ()

  (@fun call_with ()
    ?doc    "dummy function"
    ?global t
    (@with ( ( used_port  (outfile  "/dev/nukll" ) )
             ( unused_var (instring "dummy"      ) )
             )
      (car (setof elt '(1 2 3) (evenp elt)))
      (fprintf used_port "%s\n" "Test")
      ));with ;fun

  (@fun call_without_definitions
    ( ( str ?type string )
      )
    ?doc    "dummy function"
    ?global t
    ;; Empty `@with' definitions should raise a hint
    (@with ( )
      (if (pcreMatchp "([a-z]+)" str)
          (pcreSubstitute "\\1")
        str)
      ))

  );closure

