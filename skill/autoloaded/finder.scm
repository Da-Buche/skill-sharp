;; ===============================================================================================================
;; Enhanced SKILL API Finder
;;
;; Added features :
;; - Possibility to search function documentation
;; - Fixed broken 'More Info...' links
;; - Several categories can be filtered/selected
;; - Categories are in alphabetic order
;;
;; A. Buchet - July 2025
;; ===============================================================================================================

;; =======================================================
;; Classes
;; =======================================================

;; -------------------------------------------------------
;; Categories
;; -------------------------------------------------------

(@class
  ?name    'fnd_category
  ?doc     "Categories are containers to gather .fnd files and associated functions."
  ?builder nil
  ( name  @arg @err ?type string           ?doc "Category name, this is used as table key."                    )
  ( files @arg @nil ?type ( fnd_file ... ) ?doc "fnd_file objects describing .fnd files contained in category" )
  )

(let ( ( table (makeTable t nil) )
       )

  (defmethod initializeInstance @after ( ( obj fnd_category ) @rest _ )
    "Properly store OBJ."
    (setf table[obj->name] obj)
    )

  (@fun _fnd_category_by_name
    ( ( name ?type string )
      )
    ?doc "Return category named NAME."
    ?out fnd_category
    ?global t
    (or table[name] (makeInstance 'fnd_category ?name name))
    )

  (@fun _fnd_categories ()
    ?doc "Return all defined fnd_category objects."
    ?out ( fnd_category ... )
    ?global t
    (@table_elements table)
    )

  )

;; -------------------------------------------------------
;; Files
;; -------------------------------------------------------

(@class
  ?name    'fnd_file
  ?doc     "Container to gather function objects."
  ?builder nil
  ( path      @arg      ?type string               ?doc ".fnd file path."                                   )
  ( functions @arg @nil ?type ( fnd_function ... ) ?doc "Function syntax, it details arguments and output." )
  )

;; -------------------------------------------------------
;; Functions
;; -------------------------------------------------------

(@class
  ?name    'fnd_function
  ?doc     "Function name, syntax and description."
  ?builder nil
  ( name        @arg ?type string ?doc "Function name."                                       )
  ( syntax      @arg ?type string ?doc "Function syntax, it details arguments and output."    )
  ( description @arg ?type string ?doc "Function description, it descibe its behavior."       )
  ( source      @arg ?type string ?doc "Source file containing the function, when available." )
  )

;; =======================================================
;; Core
;; =======================================================

(let ()

  (@fun fnd_list_files ()
    ?doc "Return all .fnd file paths contained in all paths from $CDS_FINDER_PATH."
    ?out ( string ... )
    ;; Browse all paths from $CDS_FINDER_PATH plus native documentation root.
    (let ( ( paths (cons (strcat (car (getInstallPath)) "/../../doc/finder")
                     (parseString (getShellEnvVar "CDS_FINDER_PATH") ":")) )
           )
      (foreach mapcan path (@unique (mapcar '@realpath paths))
        (when (isReadable (setq path (@realpath path)))
          ;; Return all .fnd files contained in path
          (parseString (car (@bash (@str "find {path} -name '*.fnd'"))) "\n")
          ));when ;foreach
      ));let ;fun

  (@fun fnd_parse_file
    ( ( path ?type string )
      )
    ?doc "Build category, file and functions objects associated to .fnd file at PATH."
    ;; Category name is the directory name of the file
    (let ( ( category (_fnd_category_by_name (@basename (@dirname path))) )
           sexp
           functions
           )
      ;; Fix backslash issues before parsing the file
      ;( port (instring (@exact_replace "\\@" (@file_contents path) "\\\\@")) )
      (@with ( (port (infile path))
               )
        (muffleWarnings
          (while (setq sexp (car (errset (lineread port))))
            ;; Fix extra list introduced by lineread
            (and (listp sexp) (listp (car sexp)) (setq sexp (car sexp)))
            ;; Make sure .fnd list is valid
            (when (and (listp sexp)
                       (stringp (car sexp))
                       )
              ;; Function name string can contain several names separated by comma and/or space.
              (foreach name (parseString (car sexp) ", ")
                (let ( ( syntax      (cadr   sexp) )
                       ( description (caddr  sexp) )
                       ( source      (cadddr sexp) )
                       )
                  (unless (stringp syntax     ) (setq syntax      "???"))
                  (unless (stringp description) (setq description "???"))
                  (unless (stringp source     ) (setq source      ""   ))
                  ;; Shape syntax
                  (@letf ( ( (rexMagic) t )
                           )
                    (setq syntax (pcreReplace (pcreCompile "^([^\\(]+)") syntax "<b>\\1</b>" 1))
                    (setq syntax (pcreReplace (pcreCompile "(=>.+)$"   ) syntax "<b>\\1</b>" 1))
                    )
                  ;; Filter out ... from 'caar, caaar, caadr, cadr, caddr, cdar, cddr, ...'
                  (unless (and (equal name "...") (equal "Core_SKILL" category->name))
                    (push
                      (makeInstance 'fnd_function ?name name ?syntax syntax ?description description ?source source)
                      functions
                      ));push ;unless
                  ));let ;foreach
              ));when ;while
          ));with ;muffleWarnings
      (pushf (makeInstance 'fnd_file ?path path ?functions functions) category->files)
      ));let ;fun

  (@fun _fnd_browse_files ()
    ?doc "Browse .fnd files to build associated categories, files and functions objects."
    ?out t
    ?global t
    (@letf ( ( (status keepNLInString) t)
             )
      (mapcar fnd_parse_file (@unique (fnd_list_files)))
      t));letf ;fun

  );closure

;; =======================================================
;; GUI
;; =======================================================

(let ( (i 0)
       )

  (@fun search_callback
    ( ( _field ?type field )
      ( form   ?type form  )
      @rest _ )
    ?doc "Find all functions matching search value. Update results accordingly."
    (letseq ( ( str   form->search_field->value                            )
              ( match (pcreCompile str (pcreGenCompileOptBits ?caseLess (not (pcreMatchp "[A-Z]" str 0)))) )
              report_choices )
      ;; Fetch functions matching input
      (foreach category (_fnd_categories)
        (foreach file category->files
          (foreach function file->functions
            (when (or (pcreMatchp match function->name       )
                      (pcreMatchp match function->syntax     )
                      (pcreMatchp match function->description)
                      )
              ;; Build associated report field choice
              (push
                (list
                  function->name
                  (buildString (parseString category->name "_" t) " ")
                  (@basename file->path)
                  function->syntax
                  function->description
                  function->source
                  )
                report_choices
                )
              ));when match ;foreach function
          ));foreach file ;foreach category
      ;; Fill report field using fetched functions
      (setf form->results_field->choices report_choices)
      ));let ;fun

  (@fun select_callback
    ( ( _field ?type field )
      ( form   ?type form  )
      @rest _ )
    ?doc "Show selected function description."
    ;; Fetch selected value, prevent deselection
    (letseq ( ( index  (or (car form->results_field->value    )
                           (car form->results_field->lastValue)
                           form->results_field->last_index
                           ) )
              ( choice (nth index form->results_field->choices) )
              )
      (setf form->description_field->value
        (@nif choice ""
          (destructuringBind ( _name _category _file syntax description _source ) choice
            ;; Choice is found, make sure it is stored as last value and update 'Description' field
            (setf form->results_field->last_index index)
            (unless form->results_field->value (setf form->results_field->value (list index)))
            (@str "{syntax}<br/><hr/>{description}")
            )))))

  (@fun double_click_callback
    ( ( _field ?type field )
      ( form   ?type form  )
      @rest _ )
    ?doc "Open selected function associated documentation."
    (let ( ( index (car form->results_field->value) )
           ( file  ""                               )
           )
      (when index
        (destructuringBind ( name _category _file _syntax _description source )
                           (nth index form->results_field->choices)
          (cond
            ;; Source file is referenced
            ( (and (@nonblankstring? source)
                   (isReadable (setq file (@realpath source)))
                   )
              ;; Try to find definition in file
              (let ( ( text (@file_contents file) )
                     ( line 1                     )
                     )
                (@letf ( ( (rexMagic) t )
                         )
                  (when (pcreMatchp
                          (lsprintf "(.*)(defun|defglobalfun|defmethod|defgeneric|@fun|@macro)\\s+%s\\s+" name) text
                          (pcreGenCompileOptBits ?caseLess nil ?dotAll t)
                          )
                    (setq line (length (parseString (pcreSubstitute "\\1") "\n" t)))
                    ));when ;letf
                ;; Open IDE, scroll to definition when found
                (ilgRunSKILLIDE ?fileList (list file))
                (ilgScrollToLocation (list 1 line))
                ))
            ;; Associated help found in .tgf files
            ( (letseq ( ( doc_root (@realpath (cdsGetInstPath "doc"))                                                                           )
                        ( doc_file (car (@bash (@str "find '{doc_root}' -name '*.tgf' | xargs grep -Em1 '\\b{name}\\b' | awk '{{print $2}}'"))) )
                        )
                (setq doc_file (car (parseString doc_file "\n")))
                (cond
                  ( (isReadable doc_file)
                    (setq file doc_file)
                    )
                  ;; From "$CDS_INST_DIR/doc/cdnshelp/cdnshelp.pdf#__WKANCHOR_3e" 'Working With Tag Files' [IC6.1.8]
                  ( (equal "$" (substring doc_file 1 1))
                    (exists path (list doc_root (strcat doc_root "/../local/doc") "$HOME/doc")
                      (isReadable (setq file (@realpath (strcat path "/" (substring doc_file 2)))))
                      ))
                  )
                (isReadable file)
                );letseq
              (hiLaunchBrowser (@str "file://{file}#{name}"))
              )
            ;; Fallback to SKILL IDE Finder Double-Click callback
            (t (and (ilgRunSKILLIDE) (_ilgShowMoreInfo (strcat name)))
              )
            )
          ));dbind ;when
      ));let ;fun

  (@fun create_form ()
    ?doc "Create the form"
    (hiCreateLayoutForm (concat 'fnd_form i++) "SKILL# API Finder"
      (hiCreateVerticalBoxLayout 'main_layout ?items (list
          ;; Input
          (hiCreateComboField ?name 'search_field ?prompt "<b>Find</b>" ?items () ?callback search_callback)
          ;; Results
          (hiCreateHorizontalBoxLayout 'results_layout ?items (list
              (hiCreateLabel ?name 'results_label ?labelText "<font size='+1' color='firebrick'><b>Results</b></font>")
              (list (hiCreateSeparatorField ?name 'results_separator) 'stretch 1)
              ));results_layout
          (hiCreateReportField
            ?name            'results_field
            ?sort            '( 0 nil )
            ?selectMode      'browse ;'single
            ?enableDeselectCB t
            ?callback         select_callback
            ?doubleClickCB    double_click_callback
            ?altRowHilight    t
            ?headers
            '( ( Name        150 left string t )
               ( Category    100 left string t )
               ( File        100 left string t )
               ( Syntax        0 left string t )
               ( Description   0 left string t )
               ( Source        0 left string t )
               )
            );hiCreateReportField
          ;; Description
          (hiCreateHorizontalBoxLayout 'description_layout ?items (list
              (hiCreateLabel ?name 'description_label ?labelText "<font size='+1' color='firebrick'><b>Description</b></font>")
              (list (hiCreateSeparatorField ?name 'description_separator) 'stretch 1)
              ));description_layout
          (hiCreateHypertextField
            ?name 'description_field
            ?hasHorizontalScrollbar nil
            )
          ));list ;main_layout
      ?buttonLayout 'Empty
      ));hiCreateFormLayout ;fun

  (@fun fnd_gui ()
    ?doc "Display enhanced 'SKILL API Finder' interface."
    ?out t|nil
    ?global t
    ;; Parse fnd files to build associated objects
    (unless (_fnd_categories) (_fnd_browse_files))
    (let ( ( form (create_form) )
           )
      (search_callback form->search_field form)
      (hiDisplayForm form)
      ));let ;fun

  );closure

;*/

