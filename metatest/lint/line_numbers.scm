;; Make sure that Lint reports the proper line numbers

t
12
27 42

12 (car (setof num (@for i 0 9 i) (eq 0 num)))

(let ()

  (defun another_dummy_fun ()
    "Doc"
    (let ()
      (if t
          (progn (car (setof num (@for i 0 9 i) (eq 1 num))))
        (car
          (setof num (@for i 0 9 i) (eq 2 num))
          )
        );if
      ));let ;def

  (defun dummy_function ()
    "Doc"
    (progn
      (car (setof num (@for i 0 9 i) (eq 3 num)))
      ;; C-style intended
      (case
       (@basename (getShellEnvVar "SHELL"))
       ( "bash"
         (progn "shell is bash" (car (setof num (@for i 0 9 i) (eq 4 num))))
         )

       ( "tcsh"
         (progn "shell is tcsh" (car (setof num (@for i 0 9 i) (eq 5 num))))
         )

       ( "zsh"
         (progn "shell is zsh" (car (setof num (@for i 0 9 i) (eq 6 num))))
         )
       )
      (car (setof num (@for i 0 9 i) (eq 7 num)))
      )

    )

  );closure

t
nil
nil t nil t
nil nil
t
t

