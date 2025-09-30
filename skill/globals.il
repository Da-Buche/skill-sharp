#!/bin/csh -f
;; ## Header to run this file using SKILL interpreter.
; alias error "echo '\!:1' >/dev/stderr && exit 1"
; setenv SKILL_SHARP_ROOT `realpath $0`/../..
; setenv SKILL_SHARP_ARGV "$argv"
; $CDS_INST_DIR/tools.lnx86/dfII/bin/skill `realpath $0`
; exit $status ;

;; ===============================================================================================================
;; Load file and return all global definitions
;;
;; A. Buchet - June 2025
;; ===============================================================================================================

;; Make sure stdin is not polluted when loading the target file
(setf __globals__.poport (progn "NO_LINT" poport))
(setf __globals__.woport (progn "NO_LINT" woport))
(setguard 'poport (lambda _ (outfile "/dev/null")))
(setguard 'woport (lambda _ (outfile "/dev/null")))
(progn "NO_LINT" (setq poport nil))
(progn "NO_LINT" (setq woport nil))

;; Run before command
(when (stringp (getShellEnvVar "SKILL_SHARP_BEFORE_COMMAND"))
  (errsetstring (getShellEnvVar "SKILL_SHARP_BEFORE_COMMAND") t 'ils))

;; -------------------------------------------------------
;; Store current environment
;; -------------------------------------------------------

(setf __globals__.functions (makeTable t nil))
(setf __globals__.variables (makeTable t nil))
(setf __globals__.scheme    (makeTable t nil))
(setf __globals__.classes   (makeTable t nil))
(setf __globals__.symbols   (makeTable t nil))

(foreach __sym__ (progn "NO_LINT" oblist)
  (when (and (boundp '__sym__) (neq 'oblist __sym__) (neq '__globals__ __sym__))
    (and (isCallable __sym__                ) (setf __globals__.functions[__sym__] (getd    __sym__                    )))
    (and (boundp __sym__                    ) (setf __globals__.variables[__sym__] (symeval __sym__                    )))
    (and (boundp __sym__ (schemeTopLevelEnv)) (setf __globals__.scheme[   __sym__] (symeval __sym__ (schemeTopLevelEnv))))
    (and (findClass __sym__                 ) (setf __globals__.classes[  __sym__] (findClass __sym__                  )))
    (and (get __sym__ ?                     ) (setf __globals__.symbols[  __sym__] (get __sym__ '??                    )))
    ))

;; -------------------------------------------------------
;; Load files and run init command
;; -------------------------------------------------------

;; Load files
(foreach __file__ (parseString (getShellEnvVar "SKILL_SHARP_ARGV") " ")
  (assert (isFile     __file__) "%A is not a valid file path" __file__)
  (assert (isReadable __file__) "%A is not readable"          __file__)
  (loadi __file__)
  )

;; Run init command
(when (stringp (getShellEnvVar "SKILL_SHARP_INIT_COMMAND"))
  (errsetstring (getShellEnvVar "SKILL_SHARP_INIT_COMMAND") t 'ils)
  )

;; -------------------------------------------------------
;; Report differences between saved and updated environment
;; -------------------------------------------------------

(setf __globals__.new_functions nil)
(setf __globals__.new_variables nil)
(setf __globals__.new_scheme    nil)
(setf __globals__.new_classes   nil)
(setf __globals__.new_symbols   nil)

(foreach __sym__ (progn "NO_LINT" oblist)
  (when (and (boundp '__sym__) (neq 'oblist __sym__) (neq '__globals__ __sym__))
    (and (isCallable __sym__                )                            (nequal (getd    __sym__                    ) __globals__.functions[__sym__]) (pushf __sym__ __globals__.new_functions) )
    (and (boundp __sym__                    )                            (nequal (symeval __sym__                    ) __globals__.variables[__sym__]) (pushf __sym__ __globals__.new_variables) )
    (and (boundp __sym__ (schemeTopLevelEnv)) (not (isCallable __sym__)) (nequal (symeval __sym__ (schemeTopLevelEnv)) __globals__.scheme[   __sym__]) (pushf __sym__ __globals__.new_scheme   ) )
    (and (findClass __sym__                 )                            (nequal (findClass __sym__                  ) __globals__.classes[  __sym__]) (pushf __sym__ __globals__.new_classes  ) )
    (and (get __sym__ ?                     )                            (nequal (get __sym__ '??                    ) __globals__.symbols[  __sym__]) (pushf __sym__ __globals__.new_symbols  ) )
    ))

;; Print difference between the two states
(sstatus printinfix ())
(foreach names (if (equal "TRUE" (getShellEnvVar "SKILL_SHARP_GLOBALS_SHOW_PROPS"))
                   (list __globals__.new_functions __globals__.new_variables __globals__.new_scheme __globals__.new_classes __globals__.new_symbols)
                 (list __globals__.new_functions __globals__.new_variables __globals__.new_scheme __globals__.new_classes)
                 )
  (println (sort names 'alphalessp) __globals__.poport)
  )

(exit 0)

;*/

