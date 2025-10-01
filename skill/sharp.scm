#!/bin/csh -f
;; ## Header to run this file using SKILL interpreter.
; alias error "echo '\!:1' >/dev/stderr && exit 1"
; setenv SKILL_SHARP_ROOT `realpath $0`/../..
; setenv SKILL_SHARP_ARGV "$argv"
; test 1 = "$?SKILL_INTERPRETER" || setenv SKILL_INTERPRETER "$CDS_INST_DIR/tools.lnx86/bin/cdsmps"
; "$SKILL_INTERPRETER" `realpath $0`
; exit $status ;

;; ===============================================================================================================
;; SKILL Sharp main script.
;; This utility allows to call all sub-commands.
;;
;; A. Buchet - April 2025
;; ===============================================================================================================

;; =======================================================
;; Load SKILL#
;; =======================================================

(unless (errset (load (simplifyFilename "$SKILL_SHARP_ROOT/skill/loader.scm")))
  (fprintf (symeval 'errport) "Unable to read SKILL# loader!\n")
  (exit 1)
  )

;; =======================================================
;; Description and arguments
;; =======================================================

(@script_set_description "Run SKILL Sharp comamnds." )
(@script_set_version     "0.0.0"                     )

(@script_add_argument
  ?name "command"
  ?doc  "Sharp command to be executed. Use 'help' for a list of available commands."
  )

(@script_add_argument
  ?name   "args"
  ?doc    "Command arguments"
  ?action 'store_rest
  )

;; =======================================================
;; Define commands
;; =======================================================

(let ( ( commands (makeTable t nil) )
       )

  (@fun @sharp_run_command
    ( ( name ?type string      )
      ( args ?type (string ...) )
      )
    ?doc    "Run command names NAME"
    ?global t
    ?out    nil
    (let ( ( callback commands[name] )
           )
      (@assert callback "'{name}' is not valid, available commands are: {(@sort commands[?])}")
      (apply callback args)
      ))

  (@fun add_command
    ( ( name     ?type string   )
      ( callback ?type callable )
      )
    ?doc "Define command named NAME running CALLBACK."
    ?out nil
    (setf commands[name] callback)
    nil);fun

  ;; -------------------------------------------------------
  ;; Help
  ;; -------------------------------------------------------

  (add_command "help"
    (lambda _
      (_\@script_show_help)
      ))

  ;; -------------------------------------------------------
  ;; Lint
  ;; -------------------------------------------------------

  (add_command "lint"
    (lambda ( @rest args )
      (@debug "Running Lint on {args}")
      (@exit (if (@lint ?files (@skill_files args)) 0 1))
      ))

  ;; -------------------------------------------------------
  ;; Test
  ;; -------------------------------------------------------

  (add_command "test"
    (lambda ( @rest args )
      (@debug "Running Test on {args}")
      (let ( ( source_files nil )
             ( test_files   nil )
             ( tests_status 1   )
             )
        ;; Segregate source and test files according to _test suffix
        (foreach file (@skill_files args)
          (if (pcreMatchp "_test\\.(ils?|scm)$" file)
              (push file test_files)
            (push file source_files)
            ))
        (cond
          ;; No tests files, load all provided files
          ( (not test_files) (when (@test_run_all ?test_files source_files)                          (setq tests_status 0)) )
          ( t                (when (@test_run_all ?source_files source_files ?test_files test_files) (setq tests_status 0)) )
          )
        (@exit tests_status)
        ;; Load SKILL and test files, then run tests and exit accordingly
        ;(@exit (if (@test_run_all ?files ) 0 1))
        )
      ))

  ;; -------------------------------------------------------
  ;; Globals
  ;; -------------------------------------------------------

  (add_command "globals"
    (lambda ( @rest args )
      (@debug "Running Globals on {args}")
      ;; Make sure warnings are printed to stderr
      (@letf ( ( (@woport) (@errport) )
               )
        ;; Most arguments are passed to `@globals` using shell variables
        (foreach names (@globals ?files (@skill_files args))
          (println (sort (@unique names) 'alphalessp) (@poport))
          ))
      ))

  ;; -------------------------------------------------------
  ;; Docgen
  ;; -------------------------------------------------------

  (add_command "docgen"
    (lambda ( @rest args )
      (@debug "Running Docgen on {args}")
      (@setf (@woport) (@stderr))
      (@exit (if (@docgen ?files (@skill_files args)) 0 1))
      ))

  ;; -------------------------------------------------------
  ;; Fndcheck
  ;; -------------------------------------------------------

  (@fun get_fnd_files
    ( ( paths ?type ( string ... ) )
      )
    ?doc "Return all the CDS Finder files (.fnd) found from PATHS."
    ?out ( string ... )
    (foreach mapcan path paths
      (destructuringBind (stdout _stderr _status) (@bash (@str "find {path} -name '*.fnd'"))
        (parseString stdout "\n")
        )))

  (add_command "fndcheck"
    (lambda ( @rest args )
      (@debug "Running Fndcheck on {args}")
      (@exit (if (@fndcheck ?files (get_fnd_files args)) 0 1))
      ))

  );closure


;; =======================================================
;; Run script
;; =======================================================

(when (@script_is_running?)
  (sstatus printinfix nil)
  (@script_parse_arguments)
  (letseq ( ( args_table (@script_get_arguments)    )
            ( command    args_table["command"]->value )
            ( args       args_table["args"   ]->value )
            )
    (@debug "Running Sharp {command} on {args}")
    (@exit (if (errset (@sharp_run_command command args) t) 0 1))
    ))

;*/

