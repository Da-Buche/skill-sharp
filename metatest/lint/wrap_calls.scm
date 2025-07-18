

(let ()

  (@fun another_dummy_fun ()
    ?doc    ""
    ?global t
    (@wrap (sstatus ThisStatusDoesNotExist t)
           (progn (sstatus ThisStatusDoesNotExist nil)
                  (sstatus ANOTHER_ERRORFUL_STATUS nil)
                  )
      (car (setof elt '(1 2 3) (evenp elt)))
      (status ANOTHER_ERRORFUL_STATUS)
      ));wrap ;fun

  ;; No idea why the following is not reported...
  (@fun dummy_fun ()
    ?doc    ""
    ?global t
    ;; This should raise hint to remove @wrap
    (@wrap () ()
      (println 12)
      ));wrap ;fun

  );closure


