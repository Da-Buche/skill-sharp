;; ===============================================================================================================
;; SKILL# Library Manager Customization
;;
;; A. Buchet - September 2025
;; ===============================================================================================================

(printf "SKILL# - Loading Library Manager Customization from %s\n" (get_filename (progn "NO_LINT" piport)))

;; When setting this file as the main cdsLibMgr.il it might be nice to have it load the next regular one.
;; This depends on the setup so it is disabled for now...
; ;; Load "./cdsLibMgr.il"
; (exists shell_var '( "PWD" "CWD")
;   (let ( ( path (getShellEnvVar shell_var) )
;          )
;     (and (stringp path)
;          (isFile (setq path (strcat path "/cdsLibMgr.il")))
;          (printf "SKILL# - Loading %s\n" path)
;          (load path)
;          )))


;; =======================================================
;; Create SKILL# menu
;; =======================================================

(lmgrCreateMenu "SKILL#" '( ( "label" "SKILL#" ) ) )
(lmgrAddMenuItems "" "" '( "SKILL#" ))

;; =======================================================
;; Add button to unlock any view
;; =======================================================

(lmgrCreateMenuItem "#unlock_view_separator" "separator" ())

(lmgrCreateMenuItem "#unlock_view_item" "simple"
  `( ( "label"    "# Unlock view"                            )
     ( "callback" ( "_\\@unlock_view_from_library_manager" ) )
     ) )

(lmgrAddMenuItems "SKILL#" "V" '( "#unlock_view_separator" "#unlock_view_item" ))


;; Attempt to find Library Manager items
;; Not useful yet as I defined a custom menu for SKILL#...

; (defun _\@lmgr_find_object_by_label ( label "t" )
;   "Return the name of Library Manager labelled with LABEL."
;   (car (exists name (lmgrQueryNamedObjects "")
;          (equal label (cadr (assoc "label" (lmgrGetObject name))))))
;   )

; (printf "%N\n" (_\@lmgr_find_object_by_label "Design Manager"))

; (lmgrInsertMenuItems
;   (_\@lmgr_find_object_by_label "Version Info...")
;   "V"
;   '( "#unlock_view_separator" "#unlock_view_item" )
;   )




;*/
