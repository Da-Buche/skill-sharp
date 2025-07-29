;; ===============================================================================================================
;; Functions for debugging on the fly
;;
;; A. Buchet - April 2025
;; ===============================================================================================================

(@no_lint
  (@fun @print_args ( @rest (args) )
    ?doc "Print all provided arguments."
    (@show args)
    )
  )

;; TODO - Profile macro, but this requires skillDev license
; (defmacro @profile ( @key (field ''time) @rest sexps "g" )
;   `(@wrap (profile ',(eval field))
;             (profileSummary)
;      ,@sexps));wrap ;macro

(defun @print_table (lists @key headers "lg")
  "Pretty print LISTS as a table.

LISTS is a list of lists, all lists should have the same length.
When HEADERS is non nil, print a separator line aster the first list."
  ;; Convert elements in LISTS to strings
  (setq lists
    (foreach mapcar l lists
      (foreach mapcar e l
        (@to_string e)
        )))
  ;; Calculate maximum string length for each column
  (let ( ( lengths (foreach mapcar e (car lists) (length e)) )
         )
    (foreach l (cdr lists)
      (setq lengths
        (foreach mapcar (len e) lengths l
          (max len (length e)))))
    ;; Print formatted list
    (foreach l lists
      (foreach (len e) lengths l
        (printf (lsprintf "%%%ds" len+2) e)
        )
      (newline)
      ;; Print headers when required
      (when headers
        (foreach len lengths
          (printf (lsprintf "%%-%ds" len+2) " "))
        (setq headers nil)
        (newline)
        )
      )
    ;; Return t
    t));let ;def

(let ()

  (defun to_us (seconds)
    "Return SECONDS in ms."
    (lsprintf "%.2f" 1000000*seconds))

  (defglobalfun _\@runtime (sexps lists)
    "`@runtime' macro helper."
    (@print_table
      (cons
        '("S-Expression" "User CPU Time (us)" "System CPU Time (us)" "Clock Time (us)" "Page Faults")
         (foreach mapcar (sexp l) sexps lists
           (destructuringBind (user_cpu system_cpu clock page_faults) l
             (list sexp (to_us user_cpu) (to_us system_cpu) (to_us clock) page_faults)
             ));dbind ;foreach mapcar
         )
      ?headers t
      ));def

  );closure

(@macro @runtime ( @key ( runs 1 ) @rest sexps )
  "Compare runtimes of each S-expression in SEXPS over RUNS."
  `(_\@runtime ',sexps
     ,(cons 'list
        (foreach mapcar sexp sexps
          `(measureTime (for i 1 ,runs ,sexp))
          ));foreach mapcar ;cons
     ));macro

;*/
