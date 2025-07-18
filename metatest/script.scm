#!/bin/csh -f
;; ## Header to run this file using SKILL interpreter.
; alias error "echo '\!:1' >/dev/stderr && exit 1"
; test 1 == "$?SKILL_SHARP_ROOT"               || error 'SKILL_SHARP_ROOT is not defined!'
; test -n    "$SKILL_SHARP_ROOT"               || error 'SKILL_SHARP_ROOT is not defined!'
; test -r "$SKILL_SHARP_ROOT/skill/loader.scm" || error 'Unable to read "$SKILL_SHARP_ROOT/skill/loader.scm" !'
; setenv SKILL_SHARP_ARGV "$argv"
; $CDS_INST_DIR/tools.lnx86/dfII/bin/skill `realpath $0`
; exit $status ;

;; ===============================================================================================================
;; Example script
;;
;; A. Buchet - April 2025
;; ===============================================================================================================

;; =======================================================
;; Load SKILL#
;; =======================================================

(unless (errset (load (simplifyFilename "$SKILL_SHARP_ROOT/skill/loader.scm")))
  (fprintf (dynamic errport) "Unable to read SKILL# loader!\n")
  (exit 1)
  )

(sstatus keepNLInString t)

;; =======================================================
;; Define script help and arguments
;; =======================================================

(@script_set_description "Description of example script." )
(@script_set_version     "0.0.0"                          )


;; -------------------------------------------------------
;; Options / Key arguments
;; -------------------------------------------------------

(@script_add_argument
  ?keys   '("--verbose")
  ?action 'store_t
  ?doc    "Display more messages."
  )

(@script_add_argument
  ?keys   '("--name_with_underscore")
  ?action 'store_nil
  ?doc    "Dummy example.
With docstring over two lines."
  )

(@script_add_argument
  ?keys   '( "-n" "--no-check" )
  ?name   "check"
  ?action 'store_nil
  ?doc    "Enable checks."
  )

(@script_add_argument
  ?keys     '( "-q" "--quick" )
  ?opp_keys '( "--no-quick"   )
  ?name     "quick"
  ?action   'store_t
  ?doc      "Toggle quick mode."
  )

(@script_add_argument
  ?keys    '( "-f" "--format" )
  ?doc     "Format of the output."
  ?default "JSON"
  )

(@script_add_argument
  ?keys   '("-i" "--input-file")
  ?doc    "Path of the input file."
  ?prompt "FILE"
  )

(@script_add_argument
  ?keys   '( "-o" "--output-dir" )
  ?doc    "Name of the output directory."
  ?prompt "DIR"
  )


;; -------------------------------------------------------
;; Positional arguments
;; -------------------------------------------------------

(@script_add_argument
  ?name "required_arg"
  ?doc  "First positional argument. This one is required."
  )

(@script_add_argument
  ?name "another_required_arg"
  ?doc  "Second positional argument. This one is required."
  )

(@script_add_argument
  ?name    "optional_arg"
  ?doc     "Third positional argument. This one is optional."
  ?default ""
  )

(@script_add_argument
  ?name    "another_optional_arg"
  ?doc     "Fourth positional argument. This one is optional."
  ?default ""
  )

(@script_add_argument
  ?name   "rest_args"
  ?doc    "Other positional arguments."
  ?action 'store_rest
  )


;; =======================================================
;; Run script
;; =======================================================

(when (@script_is_running?)
  (@script_parse_arguments)
  (letseq ( ( args         (@sort (@table_elements (@script_get_arguments)) ?comp '@alphalessp ?shape (@getter 'name)) )
            ( max_name_len (minus (add1 (apply 'max (foreach mapcar arg args (length arg->name)))))                              )
            )
    (foreach arg args
      (printf "%s: %N\n" (@padd arg->name max_name_len) arg->value)
      )
    (@exit)
    ))

;*/

