;; ===============================================================================================================
;; Functions to be triggered from the Library Manager.
;;
;; A. Buchet - September 2025
;; ===============================================================================================================

; (@fun _\@unlock_view_from_library_manager
;   ( ( _item     ?type string )
;     ( lib       ?type string )
;     ( cell      ?type string )
;     ( view      ?type string )
;     ( _file     ?type string )
;     ( _category ?type string )
;     )
;   ?doc "Unlock view from the library manager."
;   (cond
;     ( (blankstrp lib ) (warn "_\\@unlock_view_from_library_manager - No lib selected..." )                     )
;     ( (blankstrp cell) (warn "_\\@unlock_view_from_library_manager - No cell selected...")                     )
;     ( (blankstrp view) (warn "_\\@unlock_view_from_library_manager - No view selected...")                     )
;     (t                 (hiEnqueueCmd (@str "(@unlock_view ?lib \"{lib}\" ?cell \"{cell}\" ?view \"{view}\")")) )
;     ))

; (@fun @unlock_view
;   ( @key
;     ( lib  ?type string )
;     ( cell ?type string )
;     ( view ?type string )
;     )
;   ?doc "Remove LIB/CELL/VIEW lock file."
;   (let ( ( path (ddGetObj lib cell view)->readPath)
;          )
;     (@show path (@bash (@str "find '{path}' -name '*cdslck*'")))
;     ))


;*/

