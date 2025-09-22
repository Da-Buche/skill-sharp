;; ===============================================================================================================
;; This is a dummy example for the custom Lint, it will probably be deleted afterwards...
;;
;; A. Buchet - September 2025
;; ===============================================================================================================


t
12
27 42

12 (car (setof num (@for i 0 9 i) (evenp num)))

(let ()

  (defun dummy_function ()
    "Doc"
    (to_be_reported)
    case( (@basename (getShellEnvVar "SHELL"))
      ( "bash"
        (progn "shell is bash" (to_be_reported))
        )

      ( "tcsh"
        (progn "shell is tcsh" (to_be_reported))
        )

      ( "zsh"
        (progn "shell is zsh" (to_be_reported))
        )
      )
    (car (setof num (@for i 0 9 i) (oddp num)))
    )

  );let

t
nil
nil t nil t
nil nil
t
t

;*/


