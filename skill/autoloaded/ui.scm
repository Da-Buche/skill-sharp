;; ===============================================================================================================
;; Helper functions to generate nice User Interfaces
;;
;; A. Buchet - August 2025
;; ===============================================================================================================

;; =======================================================
;; Field to select a color
;; =======================================================

(@fun @get_available_colors
  ( @key
    ( tech_files ?type ( tech_file ... ) ?def (@tech_files) )
    ( sorted     ?type t|nil             ?def nil           )
    ( unique     ?type t|nil             ?def nil           )
    )
  ?doc "List layer display colors available in TECH_FILE."
  ?out ( string ... )
  ?memoize t
  (let ( ( table (makeTable t nil ) )
         )
    (foreach tech_file tech_files
      (foreach lp tech_file->lps
        (@when (drFindPacket "display" (techGetLPPacketName lp))
          ?var packet
          (destructuringBind ( _display _packet _fill_style _line_style fill_color _line_color ) packet
            (setf table[fill_color] t)
            ));dbind ;when
        ));foreach lp ;foreach tech_file
    (let ( ( colors table[?] )
           )
      ;; Sort and filter duplicates when required
      (when sorted (setq colors (@sort colors)))
      (when unique
        (let ( ( color_by_index (makeTable t nil) )
               )
          (foreach color colors
            (let ( ( index (muffleWarnings (hiMatchColorByName color)) )
                   )
              (unless color_by_index[index] (setf color_by_index[index] color))
              ))
          (setq colors (@table_elements color_by_index))
          (when sorted (setq colors (@sort colors)))
          ))
      colors)
    ))

(@fun @color_icon
  ( ( color ?type string|( integer integer integer ) )
    ( size  ?type integer ?def 16                    )
    )
  ?doc "Return an icon fully filled by COLOR."
  ?out ( ptrnum integer integer )
  ?strict  t
  ?memoize t
  (let ( ( color_array (hiCreateColorArray)                                                 )
         ( color_index (if (stringp color) (hiMatchColorByName color) (hiMatchColor color)) )
         )
    ;; Color array has 26 items each described by a letter: "a" corresponds to 0
    (setf color_array[0] color_index)
    (hiStringToIcon color_array (@repeat_str "a" size*size) size size)
    ));let ;fun

(@fun @color_field
  ( @key
    ( tech_files ?type ( tech_file ... ) ?def (@tech_files) )
    @rest ( args )
    )
  ?doc "Return a field to select a color.
Except for ?choices all arguments are inherited from `hiCreateCyclicField'."
  ?out field
  (apply 'hiCreateCyclicField
    ?choices
    (foreach mapcar color (@get_available_colors ?tech_files tech_files ?sorted t ?unique t)
      (list color (@color_icon color)))
    args))

;*/
