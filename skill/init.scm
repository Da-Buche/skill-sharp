;; ===============================================================================================================
;; Load all SKILL# files.
;; /!\ This file should be loaded by loader.scm /!\
;;
;; A. Buchet - April 2025
;; ===============================================================================================================

(when (equal "TRUE" (getShellEnvVar "SKILL_SHARP_TRACK_SOURCE"))
  (sstatus debugMode   t)
  (sstatus keepSrcInfo t)
  )

;; =======================================================
;; Mising `setf' helpers
;; =======================================================

(unless (isCallable 'setf_fdoc)
  (defun setf_fdoc ( doc name "ts" )
    "`setf' helper for `fdoc'"
    (setf (get name '_fdoc) doc)
    )
  (setf (get 'setf_fdoc '_fdoc) "`setf' helper for `fdoc'")
  )

(unless (isCallable 'setf_rexMagic)
  (defun setf_rexMagic ( bool "g" ) "`setf' helper for `rexMagic'." (rexMagic bool))
  (setf (fdoc 'setf_rexMagic) "`setf' helper for `rexMagic'")
  )


;; =======================================================
;; Debugging functions
;; =======================================================

(let ( ;; Name / value pairs for `status' switches
      ( status_switches
        `( ( optimizeTailCall  ( nil on nil  off ,(status optimizeTailCall) doc "Enable debugging"                                                     ) )
           ( debugMode         ( nil on t    off ,(status debugMode       ) doc "Enable debugging"                                                     ) )
           ( traceArgs         ( nil on t    off ,(status traceArgs       ) doc "Display functions arguments in stack trace"                           ) )
           ( stacktrace        ( nil on 15   off ,(status stacktrace      ) doc "Stack trace X levels"                                                 ) )
           ( stacktraceDump    ( nil on t    off ,(status stacktraceDump  ) doc "Print environment (local variables) in stack trace"                   ) )
           ;; Source tracing is disabled as it uses skillDev license
          ;( sourceTracing     ( nil on t    off ,(status sourceTracing   ) doc "Try to point errors to file and line (for files loaded in debugMode)" ) )
           ( profCount         ( nil on t    off ,(status profCount       ) doc "Count the number of calls per function when profiling"                ) )
           ( verboseLoad       ( nil on t    off ,(status verboseLoad     ) doc "Print full path of loaded files in CIW"                               ) )
           ( verboseNamespace  ( nil on t    off ,(status verboseNamespace) doc "Enable printing of warnings related to SKILL namespaces"              ) )
           ))
      ;; Name / value pairs for global SKILL variables
      ( global_variables
        '( ( tracelength       ( nil on 50   off ,(and (boundp tracelength) (symeval 'tracelength)) ) )
           ( _stacktrace       ( nil on 50   off ,(and (boundp _stacktrace) (symeval '_stacktrace)) ) )
           ))
      ;; Debugging status
      ( debug_bool nil )
      )

  (progn "NO_LINT"
    (defun _sstatus (flag val)
      "Call `sstatus` while evaluating arguments and without triggering Lint `sstatus' check."
      (funcall 'sstatus flag val)))


  (defglobalfun @set_debug ( bool "g" )
    "Set debugging parameters according to BOOL."
    (setq debug_bool bool)
    ;; Set `status' switches
    (foreach pair status_switches
      (destructuringBind (name dpl) pair
        (_sstatus name (get dpl (if bool 'on 'off)))
        ))
    ;; Set global SKILL variables
    (foreach pair global_variables
      (destructuringBind (name dpl) pair
        (set name (get dpl (if bool 'on 'off)))
        ))
    ;; Return bool
    bool
    );def

  (defglobalfun @get_debug ( )
    "Get SKILL# debugging status."
    debug_bool)

  (defglobalfun _\@debug ( msg @rest args "tg" )
    "Print message only if SKILL# is in debugging mode."
    (and debug_bool (apply 'info msg args)))

  );closure

(define setf_\@get_debug @set_debug)
(setf (fdoc 'setf_\@get_debug) "`setf' helper for `@get_debug")

;; Lint waiver
(define setf_\\\@get_debug @set_debug)
(setf (fdoc 'setf_\\\@get_debug) "`setf' helper for `@get_debug")

;; Enable debugging by default
(when (equal "TRUE" (getShellEnvVar "SKILL_SHARP_DEBUG")) (@set_debug t))


;; =======================================================
;; Access default ports from Scheme
;; =======================================================

(inSkill
  ;; Waive Lint checks reporting unknown global variables.
  (progn "NO_LINT"

    (defun @piport  () "Return piport"  piport )
    (defun @poport  () "Return poport"  poport )
    (defun @woport  () "Return woport"  woport )
    (defun @errport () "Return errport" errport)
    (defun @stdout  () "Return stdout"  stdout )
    (defun @stderr  () "Return stderr"  stderr )

    (defun setf_\@piport  (port "p") "Set piport"  (setq piport  port))
    (defun setf_\@poport  (port "p") "Set poport"  (setq poport  port))
    (defun setf_\@woport  (port "p") "Set woport"  (setq woport  port))
    (defun setf_\@errport (port "p") "Set errport" (setq errport port))

    ;; Lint waivers
    ;; TODO - Fix Lint rule to check setf_helpers
    (defun setf_\\\@piport  (port "p") "Set piport"  (setq piport  port))
    (defun setf_\\\@poport  (port "p") "Set poport"  (setq poport  port))
    (defun setf_\\\@woport  (port "p") "Set woport"  (setq woport  port))
    (defun setf_\\\@errport (port "p") "Set errport" (setq errport port))
    ))


;; =======================================================
;; Load macros as soon as possible
;; =======================================================

(let ( ( magic (rexMagic) )
       )
  (unwindProtect
    (progn
      (rexMagic t)
      (load (simplifyFilename (strcat (get_filename piport) "/../macros/macro.scm"     )))
      (load (simplifyFilename (strcat (get_filename piport) "/../macros/function.scm"  )))
      (load (simplifyFilename (strcat (get_filename piport) "/../macros/class.scm"     )))
      (load (simplifyFilename (strcat (get_filename piport) "/../macros/patterns.scm"  )))
      (load (simplifyFilename (strcat (get_filename piport) "/../macros/f-strings.scm" )))
      (load (simplifyFilename (strcat (get_filename piport) "/../legacy.scm"           )))
      (load (simplifyFilename (strcat (get_filename piport) "/../pretty_print.scm"     )))
      (load (simplifyFilename (strcat (get_filename piport) "/../utils.scm"            )))
      (load (simplifyFilename (strcat (get_filename piport) "/../testing.scm"          )))
      );progn
    (rexMagic magic)
    ));unwindProtect ;let

(defun @realpath ( file "t" )
  "`simplifyFilename' wrapper to make it safe from `rexMagic' value."
  (@letf ( ( (rexMagic) t )
             )
    (simplifyFilename file)
    ))


;; =======================================================
;; Read and write files
;; =======================================================

(defun @read_file ( path "t" )
  "Read file at PATH and return its content as a string."
  (@with ( ( in_port  (infile path) )
             ( out_port (outstring  ) )
             )
    (let ( line ) (while (gets line in_port) (fprintf out_port "%s" line)));while ;let
    (getOutstring out_port)
    ));with ;def

(defun @write_file ( path string @optional (mode "w") "ttt")
  "Write STRING to file at PATH.

(Arguments order is meant to match `fprintf' one)"
  (@with ( ( port (outfile path mode) )
             )
    (fprintf port "%s" string)
    ));with ;def


;; =======================================================
;; Run Shell commands
;; =======================================================

(defun @bash ( command "t" )
  "Run COMMAND using `bash` then return a list containing generated stdout, stderr and exit status."
  ;; Writing everything to temporary files is not the most elegant way...
  ;; But it guarantees that input command is well understood
  ;; (even if it contains special characters like newline, single-quote or double-quote)
  ;; This should also work when running inside SKILL interpreter where `ipcBeginProcess' is not available
  (let ( ( cmd_file (makeTempFileName "/tmp/cmd.XXXXX") )
         ( out_file (makeTempFileName "/tmp/out.XXXXX") )
         ( err_file (makeTempFileName "/tmp/err.XXXXX") )
         ( sts_file (makeTempFileName "/tmp/sts.XXXXX") )
         )
    (unwindProtect
      (progn
        ;; Write command to cmd_file
        (@write_file cmd_file command)
        ;; Run command
        (let ( ( exit_status (sh (lsprintf "bash -c \"$(<%s)\" >%s 2>%s ; echo \"$?\" >%s ;" cmd_file out_file err_file sts_file)) )
               )
          ;; Return results
          (list (@read_file out_file) (@read_file err_file) (atoi (@read_file sts_file)))
          ));let ;progn
        ;; Properly delete associated files
      (foreach file (list cmd_file out_file err_file sts_file)
        (errset (deleteFile file) t))
      );unwindProtect
    ));let ;def

;; Here is another possible solution,
;; However, it has a few cons:
;;
;; - It requires `ipcBeginProcess' which is not available using the native SKILL Interpreter.
;;   (It is available with `cdsmps` though...)
;;
;; - Using it significantly increases the base PID number.
;;   It's not such a big deal, but keeping it for 'real' subprocesses
;;   gives an idea of how many children were started by Virtuoso.
;;
;; - When compared using `measureTime', it seems that current `@bash' implementation is faster.
;;   At least regarding clock time.
;;
;; - Current `@bash' manages better special characters inside input command.

; (when (isCallable 'ipcBeginProcess)
;   ;; From "Getting terminal data and printing it in CIW" 2023-04-27
;   ;; https://community.cadence.com/cadence_technology_forums/f/custom-ic-skill/57196/getting-terminal-data-and-printing-it-in-ciw
;   (defun @shell_output ( command )
;     "Run COMMAND then return stdout, stderr and exit status it generated."
;     (let ( (stdout (outstring) )
;            (stderr (outstring) )
;            exit_status)
;       (unwindProtect
;         (progn
;           ;; Run shell command and wait for it to finish
;           (ipcWait
;             (ipcBeginProcess command ""
;                              ;; Save stdout, stderr and exit status
;                              (lambda (_pid data) (fprintf stdout "%s" data))
;                              (lambda (_pid data) (fprintf stderr "%s" data))
;                              (lambda (_pid n   ) (setq exit_status n))
;                              ))
;           ;; Return stdout, stderr and exit status
;           (list (getOutstring stdout) (getOutstring stderr) exit_status)
;           );progn
;         ;; Properly close outstrings whatever happened before
;         (progn (close stdout) (close stderr))
;         );unwindProtect
;       ));let ;def
;     );when


;; =======================================================
;; Define loading functions
;; =======================================================

(letseq ( ( current_file (get_filename piport)                     )
          ( skill_root   (@realpath (strcat current_file "/..")) )
          ( loaded_files (makeTable t nil)                         )
          )

  ;; Define module root
  (setShellEnvVar "SKILL_SHARP_ROOT" (@realpath (strcat skill_root "/..")))

  ;; Add SKILL# to Finder
  (let ( ( cds_finder_path   (or (getShellEnvVar "CDS_FINDER_PATH") "")       )
         ( sharp_finder_path (@realpath (strcat skill_root "/../doc/finder")) )
         )
    (cond
      ;; Finder path is empty, replace it
      ( (blankstrp cds_finder_path)
        (setf (getShellEnvVar "CDS_FINDER_PATH") sharp_finder_path)
        )
      ;; Finder path already contains SKILL#, do nothing
      ( (index cds_finder_path sharp_finder_path)
        nil
        )
      ;; Add SKILL# at the end of $CDS_FINDER_PATH
      ( t
        (setf (getShellEnvVar "CDS_FINDER_PATH") (strcat cds_finder_path ":" sharp_finder_path))
        )
      ));cond ;let

  (defglobalfun @load (file @key (fun 'load) mark_only no_reload)
    "Load FILE using FUN and mark file as loaded.
If MARK_ONLY is non-nil, FILE is not loaded but marked as it was.
If NO_RELOAD is non-nil, FILE is not re-loaded if already marked."
    ;; Simplify file path
    (setq file (@realpath file))
    (let ( ( is_loaded loaded_files[file] )
           )
      ;; Mark file as loaded
      (setf loaded_files[file] t)
      (or mark_only
          (and no_reload is_loaded)
          (funcall fun file)
          );or
      ));let ;def

  ;; Mark current file and required ones as loaded
  (@load current_file                                ?mark_only t)
  (@load (strcat skill_root "/loader.scm"          ) ?mark_only t)
  (@load (strcat skill_root "/macros/macro.scm"    ) ?mark_only t)
  (@load (strcat skill_root "/macros/function.scm" ) ?mark_only t)
  (@load (strcat skill_root "/macros/class.scm"    ) ?mark_only t)
  (@load (strcat skill_root "/macros/patterns.scm" ) ?mark_only t)
  (@load (strcat skill_root "/macros/f-strings.scm") ?mark_only t)
  (@load (strcat skill_root "/legacy.scm"          ) ?mark_only t)
  (@load (strcat skill_root "/pretty_print.scm"    ) ?mark_only t)
  (@load (strcat skill_root "/utils.scm"           ) ?mark_only t)
  (@load (strcat skill_root "/testing.scm"         ) ?mark_only t)

  ;; Load Lint rules while redirecting info messages to `outstring'
  (let ( text line )
    (@with ( ( port (outstring) )
               )
      (@letf ( ( (@poport) port )
                 )
        (@load (strcat skill_root "autoloaded/lint.scm"))
        (setq text (getOutstring port))
        ))
    ;; Print remaining output while filtering 'INFO (LoadFile) lines'
    ;; If warning or errors are printed as well, this might modify print order.
    ;; But redirecting poport, woport and errport to split them again afterwards
    ;; is over-complicated for such a small benefit.
    (@with ( ( port (instring text) )
               )
      (while (gets line port)
        (unless (or (equal "Loading skillLint.cxt"         (substring line 1 21))
                    (equal "INFO (LoadFile): Loading file" (substring line 1 29))
                    )
          (fprintf (@poport) "'%s'" line)
          ));unless ;while
      ));with ;let

  ;; Load the rest of the module
  (foreach file (parseString (car (@bash (lsprintf "find %s/autoloaded -name '*.scm'" skill_root))) "\n")
    ;(info "Loading file: %s\n" file)
    (@load file ?no_reload t)
    )

  );closure


;; =======================================================
;; Make sure module was fully loaded
;; =======================================================

;; TODO - Make sure module is fully loaded
;(assert (isCallable 'skill_sharp) "Unable to load `skill_sharp' module properly")

;*/
