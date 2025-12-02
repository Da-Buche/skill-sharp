;; ===============================================================================================================
;; ABE functions wrappers and more.
;; This code is a modified version of my private software dedicated to ADI.
;;
;; A. Buchet - August 2025
;; ===============================================================================================================

(let ( current_cellview current_window )

  (@fun @abe_init
    ( @key
      ( cellview        ?type cellview             )
      ( window          ?type widnow|nil  ?def nil )
      ( no_interruption ?type t|nil       ?def nil )
      ( depth           ?type integer|nil ?def nil )
      ( threads         ?type integer|nil ?def nil )
      @rest _
      )
    ?doc "`abeInit` wrapper allowing to fetch session cellview using `@abe_cellview`."
    ?out t|nil
    ?global t
    (_\@abe_net_clear_table)
    (let ( args )
      (when depth   (setq args (constar ?depth   depth   args)))
      (when threads (setq args (constar ?threads threads args)))
      (@when (apply 'abeInit cellview ?doInterrupts (not no_interruption) args)
        ?var res
        (setq current_cellview cellview)
        (setq current_window   window  )
        res
        )
      ));let ;fun

  (@fun @abe_done ()
    ?doc "`abeDone` wrapper allowing to fetch session cellview using `@abe_cellview`"
    ?global t
    (_\@abe_net_clear_table)
    (setf current_cellview nil)
    (setq current_window   nil)
    (abeDone)
    )

  (@fun @abe_cellview ()
    ?doc "Return cellview set in current ABE session."
    ?global t
    (or current_cellview
        (error "No current cellview in ABE session. Are you sure current session was started with `@abe_init`?")
        ))

  (@fun @abe_window ()
    ?doc "Return window set in current ABE session."
    ?global t
    (or current_window
        (geGetCellViewWindow current_cellview)
        (error "No current window in ABE session. Are you sure current session was started with `@abe_init`?")
        ))

  )

;; =======================================================
;; Special Layers
;; =======================================================

(@fun @abe_cellview_box
  ( ( cellview ?type cellview ?def (@abe_cellview) )
    )
  ?doc "Return ABE layer built from CELLVIEW bounding box."
  (let ( ( layer  (abeNewLayer) )
         )
    (abeLayerOrPtArray (@box_to_points cellview->bBox) layer)
    layer
    ))

(@fun @abe_cellview_boundary
  ( ( cellview ?type cellview ?def (@abe_cellview) )
    )
  ?doc "Return ABE layer built from CELLVIEW boundary.
If cellview has no boundary, its bounding box is used instead."
  (let ( ( layer  (abeNewLayer) )
         )
    (abeLayerOrPtArray
      (or cellview->prBoundary->points (@box_to_points cellview->bBox))
      layer)
    layer
    ))

(@fun @abe_view_box
  ( ( window ?type window ?def (@abe_window) )
    )
  ?doc "Return ABE layer built from WINDOW bounding box."
  (let ( ( layer  (abeNewLayer) )
         )
    (abeLayerOrPtArray (@box_to_points (hiGetViewBBox window)) layer)
    layer
    ))

;; =======================================================
;; ABE Operations
;; =======================================================

;; The following functions are defined to make ABE calls
;; more Lispy, i.e. they return a newly generated
;; layer with the operation results instead of modifying
;; an exisiting one.

(@fun @abe_or
  ( ( layer0 ?type abeLayer )
    @rest
    ( layers ?type ( abeLayer ... )|nil )
    )
  ?doc "Return an ABE layer containing LAYERS union.
This is `abeLayerOr' greedy wrapper."
  ?out abeLayer
  (let ( ( layer_out (abeNewLayer) )
         )
    (foreach layer (cons layer0 layers)
      (abeLayerOr layer layer_out ?queue t)
      )
    (abeRunQueue)
    layer_out
    ));let ;def

;; For now it seems that the queued versions above are not more efficient.
;; Need to try with a different environment than a Centos7 Docker to run Virtuoso.
; (@fun @abe_or_no_queue
;   ( @rest
;     ( layers ?type ( abeLayer ... ) )
;     )
;   ?doc "Return an ABE layer containing LAYERS union.
; This is `abeLayerOr' greedy wrapper."
;   ?out abeLayer
;   (assert layers "@abe_or - At least one layer is required.")
;   (let ( ( layer_out (abeNewLayer) )
;          )
;     (foreach layer layers
;       (abeLayerOr layer layer_out)
;       )
;     layer_out
;     ))

; ;; DEBUG
; (let ( ( cv (@ccv) )
;        )
;   (@wrap (abeInit cv ?doInterrupts t ?threads 4)
;     (abeDone)
;     (@runtime
;       (apply '@abe_or          (foreach mapcar lpp cv->lpps (abeLayerFromCellView lpp->layerName ?purpose lpp->purpose)))
;       (apply '@abe_or_no_queue (foreach mapcar lpp cv->lpps (abeLayerFromCellView lpp->layerName ?purpose lpp->purpose)))
;       (apply '@abe_and         (foreach mapcar lpp cv->lpps (abeLayerFromCellView lpp->layerName ?purpose lpp->purpose)))
;       )))

(@fun @abe_and
  ( ( layer0 ?type abeLayer )
    @rest
    ( layers ?type ( abeLayer ... )|nil )
    )
  ?doc "Return an ABE layer containing LAYERS intersection.
This is `abeLayerAnd' greedy wrapper."
  ?out abeLayer
  (setq layers (cons layer0 layers))
  (let ( tmp_layers
         )
    ;; Calculate the intersection of layers by pairs in parallel to optimize runtime
    (while (cdr layers)
      (let ( out_layers
             )
        ;; Browse layers two by two
        (while layers
          (let ( ( l0 (pop layers) )
                 ( l1 (pop layers) )
                 )
            ;; Only one layer, keep it for next step
            (@nif l1 (push l0 out_layers)
              ;; Two layers, calculate the intersection
              (let ( ( tmp_layer (abeNewLayer) )
                     )
                (abeLayerAnd l0 l1 tmp_layer ?queue t)
                (push tmp_layer tmp_layers)
                (push tmp_layer out_layers)
                ));let ;nif
            ));let ;while
        ;; Keep intersection layers as input ones for next step
        (abeRunQueue)
        (setq layers out_layers)
        ));let ;while
    ;; Clear temporary layers and return the intersection layer
    (mapc 'abeRemoveLayer (cdr tmp_layers))
    (car tmp_layers)
    ));let ;def

(@fun @abe_and_not
  ( ( layer0 ?type abeLayer )
    ( layer1 ?type abeLayer )
    @rest
    ( layers ?type ( abeLayer ... ) )
    )
  ?doc "Return an ABE layer containing LAYER0 without all the shapes from LAYER1 and all the other LAYERS.
This is `abeLayerAndNot` greedy wrapper."
  ?out abeLayer
  (let ( ( layer_out (abeNewLayer) )
         )
    (abeLayerAndNot layer0 (if layers (apply '@abe_or layer1 layers) layer1) layer_out)
    layer_out
    ))

(let ( ( tmp_layers () )
       )

  (@fun xor
    ( ( layer0 ?type abeLayer )
      ( layer1 ?type abeLayer )
      )
    ?doc "`abeLayerXor` wrapper."
    (let ( ( layer_out (abeNewLayer) )
           )
      (abeLayerXor layer0 layer1 layer_out)
      (push layer_out tmp_layers)
      layer_out
      ));let ;fun

  (@fun @abe_xor
    ( ( layer0 ?type abeLayer )
      ( layer1 ?type abeLayer )
      @rest
      ( layers ?type ( abeLayer ... ) )
      )
    ?doc "Return an ABE layer containing shapes from LAYER0 and LAYER1 that do not overlap.
This is `abeLayerXor` greedy wrapper."
    ?out abeLayer
    ?global t
    (@wrap (setq tmp_layers ())
           (setq tmp_layers ())
      (@foldl1 xor (constar layer0 layer1 layers))
      (mapc 'abeRemoveLayer (cdr tmp_layers))
      (car tmp_layers)
      ))

  )

;; -------------------------------------------------------
;; Net Tracer
;; -------------------------------------------------------

(let ( ( abe_by_lpp_by_net nil )
       )

  (@fun _\@abe_net_clear_table ()
    ?doc "Clear table"
    ?out t
    ?global t
    (when (tablep abe_by_lpp_by_net)
      (foreach key abe_by_lpp_by_net[?]
        (let ( ( table (remove key abe_by_lpp_by_net) )
               )
          (foreach sub_key table[?]
            (remove sub_key table))
          ))
      )
    (setq abe_by_lpp_by_net nil)
    t)

  (@fun @abe_net
    ( ( net ?type string            )
      ( lpp ?type ( string string ) )
      )
    ?doc "Return an ABE layer containing shapes from LPP connected to NET in current session cellview."
    ?out abeLayer
    ?global t
    ;; Parse current cellview to fill [ABE layers by lpp by net] table when necessary
    (unless abe_by_lpp_by_net      (setq abe_by_lpp_by_net (makeTable t nil)))
    (unless abe_by_lpp_by_net[net] (setf abe_by_lpp_by_net[net] (build_table net)))
    ;; Return extracted layer or an empty one
    (or abe_by_lpp_by_net[net][lpp] (abeNewLayer))
    )

  (@fun build_table
    ( ( net ?type string )
      )
    ?doc "Parse net and fill table accordingly."
    ?out table
    ;; Create a trace using Net Tracer
    (letseq ( ( win   (or (@abe_window)
                          (@error "Unable to find window displaying current ABE session cellview: {(@lcv (@abe_cellview))}")) )
              ( trace (or (lntAddTrace win net)
                          (@error "Unable to extract net {net} trace in {win}.")) )
              ( table (makeTable t nil)   )
              ( depth (dbGetMaxHierDepth) )
              )
      ;; Fix `lntAddTrace` output which is not properly documented
      (when (and (listp trace) (integerp (car trace))) (setq trace (car trace)))
      ;; Save Trace to a temporary cellview
      (@wrap nil (lntRemoveTrace trace win)
        (@with ( ( tmp_lib (@tmp_lib) )
                 )
          (lntSaveTraces win (list trace) tmp_lib->name "trace" "layout")
          ;; Build an ABE Layer for each layer in extracted cellview
          (@with ( ( db_cv (dbOpenCellViewByType tmp_lib->name "trace" "layout" "" "a") )
                   )
            (foreach lpp db_cv->lpps
              (let ( ( abe_layer (abeNewLayer) )
                     )
                ;; Flatten vias
                (foreach via db_cv->vias (leFlattenInst via depth))
                ;; Extract shapes
                (foreach shape lpp->shapes
                  (unless (equal "polygon" shape->objType) (setq shape (leConvertShapeToPolygon shape)))
                  (abeLayerOrPtArray shape->points abe_layer)
                  )
                (setf table[(list lpp->layerName lpp->purpose)] abe_layer)
                ));let abe_layer ;foreach lpp
            );with db_cv
          ));with ;wrap
      ;; Return extracted table
      table))

  )

;; =======================================================
;; ABE Outputs
;; =======================================================

(@fun @abe_highlight
  ( ( layer ?type abeLayer )
    ( color ?type string   )
    )
  ?doc "Highlight ABE LAYER using COLOR."
  ?out t|nil
  ;; Tiles are not merged depending on Virtuoso version
  ;(abeLayerToHilightSet layer (@hilight_set ?cellview (@abe_cellview) ?color color) ?tiles t)
  (let ( ( hl_set (@hilight_set ?cellview (@abe_cellview) ?color color) )
         ( iter   (abeIslandIterator layer )                            )
         points
         )
    (while (setq points iter->next)
      (geAddHilightPolygon hl_set points)
      ))
  )

(@fun @abe_generate
  ( ( layer ?type abeLayer          )
    ( lpp   ?type ( string string ) )
    )
  ?doc "Generate ABE LAYER as LPP in current session cellview."
  ?out t|nil
  ;; Tiles are not merged depending on Virtuoso version
  ;(abeLayerToCellView layer (car lpp) ?purpose (cadr lpp))
  (let ( ( cv    (@abe_cellview)            )
         ( iter  (abeIslandIterator layer ) )
         points
         )
    (while (setq points iter->next)
      (dbCreatePolygon cv lpp points)
      ))
  )

;*/
