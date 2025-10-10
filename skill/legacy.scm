;; ===============================================================================================================
;; Here are all the legacy definitions required to make SKILL# work even using native SKILL interpreter
;;
;; A. Buchet - April 2025
;; ===============================================================================================================

(unless (isCallable 'muffleWarnings)

  ;; Inspired by A. Beckett's answer in "Reading Error messages from CIW" - Extracted 22 April 2025
  ;; https://community.cadence.com/cadence_technology_forums/f/custom-ic-skill/55545/reading-error-messages-from-ciw/1387707

  (@macro muffleWarnings ( @rest body )
    "Legacy `muffleWarnings', run BODY without printing any message to woport.
Warnings catched during evaluation can be fetched using `getMuffleWarnings'."
    `(@with ( ( tmp_port (outstring) )
                )
       (prog1
         ;; Redirect woport
         (@letf ( ( (@woport) tmp_port )
                    )
           (prog1 (progn ,@body)
             ;; Force final warning to be flushed
             (warn "")
             (getWarn)
             ));prog1 ;letf
         ;; Store catched warnings
         (let ( ( str (getOutstring tmp_port))
                warnings
                )
           (when str
             ;; TODO - Make sure legacy `muffleWarnings' and `getMuffleWarnings' have accurate behavior
             ;(setq warnings (list str))
             ;; Split warnings
             (@letf ( ( (rexMagic) nil )
                     )
               (rexCompile "*WARNING*")
               (setq str (rexReplace str "¶" 0))
               )
             (setq warnings
               ;; Remove added warning in case it was printed before being catched
               (foreach mapcon _warnings (parseString str "¶")
                 (unless (and (not (cdr _warnings)) (blankstrp (car _warnings)))
                   (list (strcat "*WARNING*" (car _warnings)))
                   ))
               )
             );when
           ;; This variable name is poorly chosen but at least it's consistent with errset.errset...
           (setf muffleWarnings.muffleWarnings warnings)
           ));prog1 ;let
       ));with ;macro

  (@fun getMuffleWarnings ()
    ?doc "Legacy `getMuffleWarnings', return warnings catched by last `muffleWarnings' call."
    ?out list
    muffleWarnings.muffleWarnings
    )

  );unless

;; =======================================================
;; Missing setf helpers
;; =======================================================

(unless (isCallable 'setf_status)
  (progn "NO_LINT" (defsetf status sstatus))
  (setf (@fdoc 'setf_status) "`setf' helper for `status'.

This allow the use of calls like (setf (status saveInlineDoc) t).

And also `letf' calls like:
(letf ( ( (status printinfix) nil )
        )
  (println '(a+b))
  )"
  ))

(defun @setf_getShellEnvVar ( val var "gt" )
  "Improved `setf_getShellEnvVar'.
This function allows to use nil as VAL.
Otherwise `getShellEnvVar' is not usable with `@letf',
as when an unset value is defined it cannot be reverted back."
  (cond
    ( (not     val) (unsetShellEnvVar var    ) )
    ( (stringp val) (setShellEnvVar   var val) )
    ( t             (error "@setf_getShellEnvVar - value is not a string: %N" val))
    ))

(unless (isCallable 'setf_getShellEnvVar)
  (define setf_getShellEnvVar (getd '@setf_getShellEnvVar)))

;*/

