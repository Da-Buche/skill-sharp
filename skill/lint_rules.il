;; ===============================================================================================================
;; Custom Lint rules are defined here.
;; Having a dedicated file is much cleaner.
;; It can be loaded directly when using `sklint`.
;; Also SK_RULE macro almost never passes Lint
;;
;; A. Buchet - September 2025
;; ===============================================================================================================

;; -------------------------------------------------------
;; Lint waiver
;; -------------------------------------------------------

{ "NO_LINT"
  (SK_RULE SK_CONTROL ( @no_lint ) t nil)
  }

{ "NO_LINT"
  (SK_RULE SK_CONTROL ( progn    ) t
    ;; Check `progn' first argument
    (unless (equal "NO_LINT" (car (SK_ARGS)))
      (foreach map sexp (SK_ARGS) (SK_CHECK_FORM sexp))
      ))
  }

;; -------------------------------------------------------
;; (car (setof ...)) to replace by (car (exists ...))
;; -------------------------------------------------------

{ "NO_LINT"
  (SK_RULE ( setof ) (equal 'car (caar (SK_FORM 2)))
    (SK_HINT CAR_SETOF "(car (setof ...)) should be replaced by (car (exists ...)): %N" (SK_FORM))
    )
  }

;; -------------------------------------------------------
;; Unknown `status` or `sstatus` calls
;; -------------------------------------------------------

;; This is required at least when running Lint from the SKILL Interpreter
;; Otherwise some valid statuses are reported as unknown
{ "NO_LINT"
  (SK_RULE SK_CONTROL ( status sstatus ) (not (errset (funcall 'status (car (SK_ARGS)))))
    (SK_ERROR UNKNOWN_STATUS_FLAG "Unknown (s)status flag: %N\n" (SK_FORM))
    )
  }


;*/

