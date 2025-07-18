(let ()

  (@fun call_with ()
    ?doc    "dummy function"
    ?global t
    (car (setof elt '(1 2 3) (evenp elt)))
    );fun

  );closure

