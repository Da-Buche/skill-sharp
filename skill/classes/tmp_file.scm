;; ===============================================================================================================
;; Context managers for temporary objects
;;
;; A. Buchet - April 2025
;; ===============================================================================================================

;; =======================================================
;; Temporary File
;; =======================================================

(@class
  ?name    '@tmp_file
  ?doc     "Define a temporary file. It is meant to be used in `@with' context manager."
  ?builder t
  ( template @arg ?init ""                               )
  ( path          ?init (@mktemp (@built_obj)->template) )
  ( port          @nil                                   )
  )

;; -------------------------------------------------------
;; Context management
;; -------------------------------------------------------

(defmethod _\@in ( ( obj @tmp_file ) )
  "Context manager when opening temporary file, nothing to do..."
  nil)

(defmethod _\@out ( ( obj @tmp_file ) )
  "Context manager when closing a temporary file, delete it"
  (when (openportp obj->port) (close obj->port))
  (deleteFile obj->path)
  )

;; -------------------------------------------------------
;; Write to temporary file
;; -------------------------------------------------------

(defmethod @write ( ( obj @tmp_file ) str)
  "Write STR to temporary file OBJ port."
  (let ( ( port (or obj->port (setf obj->port (outfile obj->path "a"))) )
         )
    (fprintf port "%s" str)
    (drain port)
    ))

;; Testing
; (@with ( ( tmp_file (@tmp_file) )
;          )
;   (println tmp_file->??)
;   (@write tmp_file "Hello World!")
;   (println (@read_file tmp_file->path))
;   )

;; =======================================================
;; Temporary Library
;; =======================================================

(@class
  ?name    '@tmp_lib
  ?doc     "Define a temporary library. It is meant to be used in `@with' context manager."
  ?builder t
  ( template @arg ?init "/tmp/tmp_lib_XXXXX"                                )
  ( path          ?init (@mktemp (@built_obj)->template t)                  )
  ( name          ?init (@basename (@built_obj)->path)                      )
  ( dd_obj        ?init (ddCreateLib (@built_obj)->name (@built_obj)->path) )
  )

(defmethod _\@in ( ( obj @tmp_lib ) )
  "Context manager when opening temporary file, nothing to do..."
  nil)

(defmethod _\@out ( ( obj @tmp_lib ) )
  "Context manager when closing a temporary file, delete it"
  (@nif (ddIsId obj->dd_obj)
        (warn "Unable to delete temporary library named {obj->name} {obj} - Not a valid ddobj : {obj->dd_obj}")
    (ddDeleteObj obj->dd_obj)
    ;; Remove pesky deletion comments in cds.lib
    (let ( ( file (ddGetStartup "cds.lib") )
           )
      (and file (isWritable file) (@bash "sed -e '/#Removed by ddDeleteObj: DEFINE tmp_lib_/d' -i {file}"))
      )))


;*/


