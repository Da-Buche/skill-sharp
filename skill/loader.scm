;; ===============================================================================================================
;; Load all SKILL# init.
;; This file is only a wrapper around `load' to set proper statuses and use `measureTime'.
;;
;; A. Buchet - June 2025
;; ===============================================================================================================

(let ( ( keepNLInString (status keepNLInString) )
       ( saveInlineDoc  (status saveInlineDoc ) )
       ( magic          (rexMagic             ) )
       )
  (progn
    (sstatus keepNLInString t)
    (sstatus saveInlineDoc  t)
    (rexMagic               t)
    )
  (unwindProtect
    (destructuringBind
        (_user_time _system_time clock_time _page_faults)
        (measureTime
          ;; Load init
          (unless (errset (load (simplifyFilename (strcat (get_filename piport) "/../init.scm"))))
            (fprintf (dynamic errport) "Unable to read SKILL# init!\n")
            (exit 1)
            ))
      (_\@debug "Loaded SKILL# in %.4ns\n" clock_time)
      );dbind(sstatus keepNLInString t)
    ;; Revert statuses
    (progn
      (sstatus keepNLInString keepNLInString)
      (sstatus saveInlineDoc  saveInlineDoc )
      (rexMagic               magic         )
      )
    );unwindProtect

  );let


;*/

