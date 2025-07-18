(progn

  (@fun dummy_fun ()
    ?doc    ""
    ?global t
    ;; This should raise hint to remove @wrap
    (@wrap () ()
      (println 12)
      ));wrap ;fun

  );progn

