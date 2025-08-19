; ;; ===============================================================================================================
; ;; File to define temporary files in context manager.
; ;;
; ;; A. Buchet - April 2025
; ;; ===============================================================================================================

; ; (@class
; ;   ?name    '@tmp_file
; ;   ?doc     "Define a temporary file. It is meant to be used in `@with' context manager."
; ;   ?builder t
; ;   ( template @arg ?init ""                               )
; ;   ( name          ?init (@mktemp (@built_obj)->template) )
; ;   ( port          @nil                                   )
; ;   )



; (defclass @tmp_file ()
;   ( (template @initarg template @initform (error "@tmp_file - ?TEMPLATE is mandatory."))
;     (name                       @initform (@mktemp (@built_obj)->template)           )
;     (port                       @initform nil                                            )
;     ))

; (defmethod initializeInstance @before ( ( obj @tmp_file) @rest _ ) (_\@set_built_obj obj) )

; (defun @tmp_file ( @key (template (error "@tmp_file - ?TEMPLATE is mandatory.")) )
;   "Builder for @tmp_file class."
;   (makeInstance '@tmp_file ?template template)
;   )


; ;; =======================================================
; ;; Context management
; ;; =======================================================

; (defmethod _\@in ( ( obj @tmp_file ) )
;   "Context manager when opening temporary file, nothing to do..."
;   nil)

; (defmethod _\@out ( ( obj @tmp_file ) )
;   "Context manager when closing a temporary file, delete it"
;   (when (openportp obj->port) (close port))
;   (deleteFile obj->name)
;   )


; ;; =======================================================
; ;; Write to temporary file
; ;; =======================================================

; (defmethod @write ( ( obj @tmp_file ) str)
;   "Write STR to temporary file OBJ port."
;   (let ( ( (or obj->port (setf obj->port (outfile obj->name "a"))) )
;          )
;     (fprintf port "%s" str)
;     (drain port)
;     ))

; ;*/
