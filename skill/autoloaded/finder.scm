;; ===============================================================================================================
;; Enhanced SKILL API Finder
;;
;; Added features :
;; - Possibility to search function name, arguments, description, or category
;; - Fixed broken 'More Info...' links
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
  ?name    '@fnd_category
  ?doc     "Categories are containers to gather .fnd files and associated functions."
  ?builder nil
  ( name  @arg @err ?type string           ?doc "Category name, this is used as table key."                    )
  ( files @arg @nil ?type ( fnd_file ... ) ?doc "fnd_file objects describing .fnd files contained in category" )
  )

(let ( ( table (makeTable t nil) )
       )

  (defmethod initializeInstance @after ( ( obj @fnd_category ) @rest _ )
    "Properly store OBJ."
    (setf table[obj->name] obj)
    )

  (@fun _\@fnd_category_by_name
    ( ( name ?type string )
      )
    ?doc "Return category named NAME."
    ?out @fnd_category
    ?global t
    (or table[name] (makeInstance '@fnd_category ?name name))
    )

  (@fun _\@fnd_categories ()
    ?doc "Return all defined @fnd_category objects."
    ?out ( @fnd_category ... )
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
    ;; (underscores are replaced by spaces)
    (let ( ( category (_\@fnd_category_by_name (buildString (parseString (@basename (@dirname path))  "_" t) " ")) )
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
                  (unless (and (equal name "...") (equal "Core SKILL" category->name))
                    (push
                      (makeInstance 'fnd_function ?name name ?syntax syntax ?description description ?source source)
                      functions
                      ));push ;unless
                  ));let ;foreach
              ));when ;while
          ));with ;muffleWarnings
      (pushf (makeInstance 'fnd_file ?path path ?functions functions) category->files)
      ));let ;fun

  (@fun _\@fnd_browse_files ()
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

  (@fun split_search
    ( ( str ?type string )
      )
    ?doc "Split STR into a mapping table."
    ?out ( ( symbol symbol string ) ... )
    (let ( res )
      ;; Parse string to find <name>=<pcre> pairs
      (foreach pcre_str '( "([a-zA-Z]+)(!?=)'([^']*?)'" "([a-zA-Z]+)(!?=)\"([^\"]*?)\"" "([a-zA-Z]+)(!?=)([^\\s]+)" )
        (let ( ( pcre (pcreCompile pcre_str) )
               )
          (while (pcreMatchp pcre str)
            (setq str (pcreReplace pcre str "" 1))
            (push (list (concat (lowerCase (pcreSubstitute "\\1"))) (concat (pcreSubstitute "\\2")) (pcreSubstitute "\\3")) res)
            );while
          ));let ;foreach
      ;; Consider remaining non-blank strings as general matches
      (foreach match (parseString str " ")
        (push (list 'general '\= match ) res)
        );foreach
      ;; Return symbol / pcre associaton table or default to one matching everything
      (or res '( ( general \= "" ) ))
      ));let ;fun

  (@test
    ?fun 'split_search
    ?doc "Infile tests to guarantee `split_search' behavior."

    (@assertion
      (split_search "")
      ?out '( ( general \= "" ) )
      )

    (@assertion
      (split_search "  ")
      ?out '( ( general \= "" ) )
      )

    (@assertion
      (split_search "a b")
      ?out '( ( general \= "b" ) ( general \= "a" ) )
      )

    (@assertion
      (split_search "^hi.*$")
      ?out '( ( general \= "^hi.*$" ) )
      )

    (@assertion
      (split_search "^hi form$")
      ?out '( ( general \= "form$" ) ( general \= "^hi" ) )
      )

    (@assertion
      (split_search "c=core")
      ?out '( ( c \= "core" ) )
      )

    (@assertion
      (split_search "category='Core SKILL|DFII SKILL' name='^hi' description=\"apply button\"")
      ?out '( (description \= "apply button") ( name \= "^hi" ) ( category \= "Core SKILL|DFII SKILL" ) )
      )

    (@assertion
      (split_search "name='^hi' name='form$' cat='DFII SKILL' layout form")
      ?out '( ( general \= "form" )( general \= "layout" ) ( cat \= "DFII SKILL" ) ( name \= "form$" ) ( name \= "^hi") )
      )

    (@assertion
      (split_search "name='^hi' name='form$' cat!='Core SKILL' layout form")
      ?out '( ( general \= "form" )( general \= "layout" ) ( cat \!\= "Core SKILL" ) ( name \= "form$" ) ( name \= "^hi") )
      )

    )

  (@fun search_callback
    ( ( _field ?type field )
      ( form   ?type form  )
      @rest _ )
    ?doc "Find all functions matching search value. Update results accordingly."
    (let ( ( matches        (split_search form->search_field->value) )
           ( report_choices ()                                       )
           ( msg            ""                                       )
           )
      (@letf ( ( (rexMagic) t )
                 )
        ;; Simplify matches
        (setq matches
          (foreach mapcar match matches
            (destructuringBind ( symbol relation pcre_str ) match
              (list symbol relation
                ;; Match is caseless if it only contains lowercase
                (pcreCompile pcre_str (pcreGenCompileOptBits ?caseLess (not (pcreMatchp "[A-Z]" pcre_str 0)) ))
                ));list ;dbind
            ));foreach ;setq
        ;; Fetch functions matching input
        (foreach category (_\@fnd_categories)
          (foreach file category->files
            (foreach function file->functions
              (when (forall match matches
                      (destructuringBind ( symbol relation pcre ) match
                        (funcall (@caseq relation ( \= '@identity ) ( \!\= 'not ) )
                          (caseq symbol
                            ( ( c cat  category      ) (pcreMatchp pcre category->name        ) )
                            ( ( n name               ) (pcreMatchp pcre function->name        ) )
                            ( ( a arg args arguments ) (pcreMatchp pcre function->syntax      ) )
                            ( ( s syntax             ) (pcreMatchp pcre function->syntax      ) )
                            ( ( o output             ) (pcreMatchp pcre function->syntax      ) )
                            ( ( d desc description   ) (pcreMatchp pcre function->description ) )
                            ( ( g general            ) (or (pcreMatchp pcre function->name       )
                                                           (pcreMatchp pcre function->syntax     )
                                                           (pcreMatchp pcre function->description)
                                                           ))
                            ( t (setq msg (@str "<font color='firebrick'><b>Not a valid target: {symbol}<br>\n\
  It should be 'name', 'category', 'syntax', or 'description'<br>\n\
(or respectively 'n', 'c', 's', or 'd').</b></font>")))
                            ));caseq ;funcall
                          ));dbind ;forall match
                ;; Build associated report field choice
                (push
                  (list
                    function->name
                    category->name
                    (@basename file->path)
                    function->syntax
                    function->description
                    function->source
                    )
                  report_choices
                  ));push ;when
              )));foreach function ;foreach file ;foreach category
        );letf
      ;; Fill report field using fetched functions
      (setf form->results_field->choices report_choices)
      ;; Update status
      (setf form->status_field->value
        (if (@nonblankstring? msg) msg
          (@str "{(length report_choices)} matches found")
          ))
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

  (@fun _\@fnd_open_documentation_callback
    ( ( _field ?type field )
      ( form   ?type form  )
      @rest _ )
    ?doc "Open selected function associated documentation."
    ?global t
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
            (t (and (ilgRunSKILLIDE) (_ilgShowMoreInfo (strcat name))) )
            );cond
          ));dbind ;when
      ));let ;fun

  (@fun _\@fnd_set_category_callback
    ( ( form    ?type form  )
      ( exclude ?type t|nil )
      )
    ?doc "Exclude selected category from search."
    ?global t
    (let ( ( index (car form->results_field->value) )
           )
      (when index
        (destructuringBind ( _name category _file _syntax _description _source )
                           (nth index form->results_field->choices)
          (when (@nonblankstring? category)
            (@letf ( ( (rexMagic) t )
                     )
              (setf form->search_field->value
                (strcat
                  (pcreReplace
                    (pcreCompile (@str "(c|cat|category)!?=({category}\\B|'{category}'|\"{category}\")"))
                    form->search_field->value "" 0)
                  " "
                  (if exclude (@str "cat!='{category}'") (@str "cat='{category}'"))
                  ));strcat ;setf
              ));letf ;when
          ));dbind ;when
      ));let ;fun

  (@fun set_tooltip
    ( ( field   ?type field  )
      ( tooltip ?type string )
      )
    ?doc "Set FIELD TOOLTIP. Return FIELD."
    ?out field
    (setf field->hiToolTip (lsprintf "
<html>
<head>
  <style>
    body   { font-size        : 12px     ; }
    table  { background-color : black    ; }
    tr.e   { background-color : #e8e8e8  ; }
    tr.o   { background-color : #ffffff  ; }
    th     { background-color : #b1b1b2  ; }
    td     { padding          : 5px      ; }
    li     { font-weight      : bold     ;
             margin-left      : -30px    ;
             white-space      : nowrap   ;
             }
 </style>
</head>
<body>
<td>%s</td>
</body>
</html>" tooltip))
    field
    )

  (@fun create_form ()
    ?doc "Create the form"
    (hiCreateLayoutForm (concat '@fnd_form i++) "SKILL# API Finder"
      (hiCreateVerticalBoxLayout 'main_layout ?items (list
          ;; -------------------------------------------------------
          ;; Input
          ;; -------------------------------------------------------
          (set_tooltip
            (hiCreateComboField
              ?name    'search_field
              ?prompt  "<b>Find</b>"
              ?callback search_callback
              ?items   '( "^hi field$"
                          "name=alphalessp"
                          "category!='Core SKILL'"
                          "category='Core SKILL|DFII SKILL'"
                          "arguments=cellview"
                          "description=\"current window display\""
                          "n=abe c='Custom Layout'"
                          )
              )
            "<b>This field contains the text to be searched in functions name, syntax (or arguments), description, or category.</b>

<ul>
  <li> It can be defined as a Perl Compatible Regular Expression [PCRE].            </li>
  <li> It is possible to select which value should match using '=' or '!='.         </li>
  <li> Each search pattern is caseless unless it has at least one uppercase letter. </li>
  <li> Several filters can be used at the same time.                                </li>
</ul>

<br/>

<table>
  <tr class='e'>  <th> Search value                             </th> <th> Description                                                          </th> </tr>
  <tr class='o'>  <td> <pre> ^abe                        </pre> </td> <td> Any value contains a word starting with 'abe'.                       </td> </tr>
  <tr class='e'>  <td> <pre>field$                       </pre> </td> <td> Any value contains a word ending with 'field'.                       </td> </tr>
  <tr class='o'>  <td> <pre>name='alphalessp'            </pre> </td> <td> Name contains 'alphalessp'.                                          </td> </tr>
  <tr class='e'>  <td> <pre>category='Core SKILL'        </pre> </td> <td> Category contains 'Core SKILL'.                                      </td> </tr>
  <tr class='o'>  <td> <pre>arguments!=[0-9]             </pre> </td> <td> Arguments does not contain numbers.                                  </td> </tr>
  <tr class='e'>  <td> <pre>category=layout name=cellview</pre> </td> <td> Category contains 'layout' & name contains 'cellview'.               </td> </tr>
  <tr class='o'>  <td> <pre>name=^abe syntax!='cell|win' </pre> </td> <td> Name starts with 'abe' and arguments do not contain 'cell' or 'win'. </td> </tr>
  <tr class='e'>  <td> <pre>category='DFII' name!=^hi    </pre> </td> <td> Category contains 'DFII' & name does not start with 'hi'.            </td> </tr>
</table>

<br/>
<br/>

<b>
To view more examples, click on the down arrow
<font size=-1><span style='background-color: firebrick; color: white;'>&nbsp;&#x25BC;&nbsp;</span></font> .<br/>
It contains predefined filters.
</b>"
            )
          ;; -------------------------------------------------------
          ;; Results
          ;; -------------------------------------------------------
          (hiCreateHorizontalBoxLayout 'results_layout ?items (list
              (hiCreateLabel ?name 'results_label ?labelText "<font size='+1' color='firebrick'><b>Results</b></font>")
              (list (hiCreateSeparatorField ?name 'results_separator) 'stretch 1)
              ));results_layout
          (set_tooltip
            (hiCreateReportField
              ?name            'results_field
              ?sort            '( 0 nil )
              ?selectMode      'browse ;'single
              ?enableDeselectCB t
              ?callback         select_callback
              ?doubleClickCB    _\@fnd_open_documentation_callback
              ?altRowHilight    t
              ?headers
              '( ( Name        200 left string t )
                 ( Category    100 left string t )
                 ( File        100 left string t )
                 ( Syntax        0 left string t )
                 ( Description   0 left string t )
                 ( Source        0 left string t )
                 )
              );hiCreateReportField
            "<b>This field shows all the functions matching 'Find' input.</b>
<ul>
  <li> Click on a function to show its description.                 </li>
  <li> Double&#8209;click to view function detailled documentation. </li>
  <li> Right-click to open context menu.                            </li>
</ul>"
            )
          ;; ---------------------------------------------------------------------------------------------------------------
          ;; Description
          ;; ---------------------------------------------------------------------------------------------------------------
          (hiCreateHorizontalBoxLayout 'description_layout ?items (list
              (hiCreateLabel ?name 'description_label ?labelText "<font size='+1' color='firebrick'><b>Description</b></font>")
              (list (hiCreateSeparatorField ?name 'description_separator) 'stretch 1)
              ));description_layout
          (hiCreateHypertextField
            ?name 'description_field
            ?hasHorizontalScrollbar nil
            )
          ;; -------------------------------------------------------
          ;; Status
          ;; -------------------------------------------------------
          (hiCreateLabel ?name 'status_field ?labelText "Ready")
          ));list ;main_layout
      ?buttonLayout 'Empty
      ?initialSize  '( 330 465 )
      ));hiCreateFormLayout ;fun

  (@fun @fnd_gui ()
    ?doc "Display enhanced 'SKILL API Finder' interface."
    ?out t|nil
    ?global t
    ;; Parse fnd files to build associated objects
    (unless (_\@fnd_categories) (_\@fnd_browse_files))
    (let ( ( form (create_form) )
           )
      (hiInstantiateForm form)
      ;; Add context menu to results field
      (setf form->results_field->hiContextMenu
        (hiCreateSimpleMenu
         (concat form->hiFormSym '_context_menu)
         ""
         '( "Open Documentation" "Exclude Category" "Focus Category" )
          (list
           (@str "(_\\@fnd_open_documentation_callback {form->hiFormSym}->results_field {form->hiFormSym} t)")
           (@str "(_\\@fnd_set_category_callback {form->hiFormSym} t  )")
           (@str "(_\\@fnd_set_category_callback {form->hiFormSym} nil)")
           )))
      (search_callback form->search_field form)
      (hiDisplayForm form -1:-1)
      ));let ;fun

  );closure

;; =======================================================
;; Replace Native Finder
;; =======================================================

(let ()

  (@fun label_equal?
    ( ( str0 ?type string )
      ( str1 ?type string )
      )
    ?doc "Return t if STR0 and STR1 are identical menu (or menu item) labels."
    ?out t|nil
    (equal
      (lowerCase (@exact_replace "&" str0 ""))
      (lowerCase (@exact_replace "&" str1 ""))
      ))

  (@fun find_menu_by_label
    ( ( str    ?type string ?doc "Label displayed by one of the banner menus in WINDOW." )
      ( window ?type window ?doc "Window containing the banner menu."                    )
      )
    ?doc "Return the first CIW menu whose label matches STR."
    ?out hiMenu
    (or (prog ()
          (foreach menu_sym (hiGetBannerMenus window)
            (let ( ( menu (symeval menu_sym) )
                   )
              (when (label_equal? str menu->_menuTitle) (return menu))
              );let
            ));foreach ;prog
        (@error "Unable to find menu labelled \"{str}\" amongst {window} banner menus: {(hiGetBannerMenus window)}")
        ));or ;fun

  (@fun find_item_by_label
    ( ( str  ?type string ?doc "Label displayed by one of the items in MENU." )
      ( menu ?type hiMenu ?doc "Menu containing the item."                    )
      )
    ?doc "Return the first CIW menu whose label matches STR."
    ?out hiMenuItem
    (or (prog ()
          (foreach item_sym (hiGetMenuItems menu)
            (let ( ( item (get menu item_sym) )
                   )
              (and
                (stringp item->hiItemText)
                (label_equal? str item->hiItemText)
                (return item)
                ));and ;let
            ));foreach ;prog
        (@error "Unable to find item labelled \"{str}\" amongst menu items: {(hiGetMenuItems menu)}")
        ))

  (@fun replace_item
    ( ( menu     ?type hiMenu     )
      ( item     ?type hiMenuItem )
      ( new_item ?type hiMenuItem )
      )
    ?doc "Replace ITEM by NEW_ITEM in MENU."
    (letseq ( (item_sym item->hiMenuItemSym)
              (position (or (prog ( ( i -1 ) ) (foreach sym (hiGetMenuItems menu) i++ (and (eq sym item_sym) (return i))))
                            (@error "Unable to find item {item} in menu {menu}.")) )
              )
      (hiDeleteMenuItem menu item_sym         )
      (hiInsertMenuItem menu new_item position)
      ));let ;fun

  (@fun _\@fnd_replace_native_finder_in_ciw ()
    ?doc "Replace 'SKILL API Finder' by 'SKILL# API Finder' in CIW->Tools menu."
    ?global t
    (letseq ( ( menu (find_menu_by_label "Tools"            (hiGetCIWindow)) )
              ( item (find_item_by_label "SKILL API Finder" menu           ) )
              )
      (replace_item menu item
        (hiCreateMenuItem
          ?name item->hiMenuItemSym
          ?itemText item->hiItemText
          ?itemIcon (hiLoadIconData (@realpath "$SKILL_SHARP_ROOT/pictures/icons/sharp.png"))
          ?callback "(if (equal \"TRUE\" (getShellEnvVar \"SKILL_SHARP_KEEP_NATIVE_FINDER\")) (startFinder) (@fnd_gui))"
          ))
      ));let ;fun

  );closure

;; Replacing SKILL API Finder by force.
;; This is a bit intrusive but this finder supports all the native features and adds more.
(when (isCallable 'hiEnqueueCmd) (hiEnqueueCmd "(_\\@fnd_replace_native_finder_in_ciw)"))

;*/

