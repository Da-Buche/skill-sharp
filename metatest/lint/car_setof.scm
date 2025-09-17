(let ()

  (defglobalfun car_setof ()
    "dummy function for lint purposes"
    (car (setof elt (list 1 2 3) (evenp elt)))
    );fun

  );closure

