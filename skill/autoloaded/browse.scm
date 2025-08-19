;; ===============================================================================================================
;; Functions to recursively browse cellviews
;;
;; A. Buchet - August 2025
;; ===============================================================================================================

(let ( opened_views_table
       sch_view_names
       )

  (@fun open
    ( ( lcv  ?type ( string string string) )
      )
    ?doc "Return opened cellview described by LCV."
    ?out cellview
    (or opened_views_table[lcv]
        (setf opened_views_table[lcv] (apply 'dbOpenCellViewByType lcv))
        ))

  (@fun browse
    ( (cellview ?type cellview)
      )
    ?doc "Recursively browse CELLVIEW and fill opened_views_table accordingly."
    ;; Mark current cellview as done
    (foreach header cellview->instHeaders
      (let ( ( lcv (@lcv header) )
             )
        (unless opened_views_table[lcv]
          (let ( ( sub_cellview (open lcv) )
                 )
            (case sub_cellview->cellViewType
              ;; Header describes a symbol, find first associated view.
              ( "schematicSymbol"
                (letseq ( ( lib   (car  lcv) )
                          ( cell  (cadr lcv) )
                          ( view  (car (exists view sch_view_names (ddGetObj lib cell view) )))
                          )
                  (when view (browse (open (list lib cell view))))
                  ))
              ;; TODO - Support config views

              ;; Any other view type, browse it directly
              ( t (browse sub_cellview) )
            ));case ;let
          ));unless ;let
      ));foreach header ;fun

  (@fun @list_lcv
    ( @key
      ( cellview ?type dbobject )
      @rest _
      )
    ?doc "Return all the Libray, Cell, View tuples used in CELLVIEW."
    ?out ( ( string string string ) ... )
    ?global t
    ?strict t
    ;; Reset table properly
    (@wrap (progn
             (setq opened_views_table (makeTable t nil))
             (setq sch_view_names (parseString (schGetEnv "viewNameList") " ,"))
             )
           (progn
             (mapcar 'dbClose (@table_elements opened_views_table))
             (setq opened_views_table nil)
             (setq sch_view_names     nil)
             )
      ;; Use tail-call optimization
      (@letf ( ( (status debugMode       ) nil )
               ( (status optimizeTailCall) t   )
               )
        ;; Browse cellviews recursively, return the list of opened ones.
        (browse (open (@lcv cellview)))
        opened_views_table[?]
        ));letf ;wrap
    );fun

  );closure

;*/

