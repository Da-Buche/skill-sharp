;; ===============================================================================================================
;; GUI example to communicate with Python using TCP server
;;
;; A. Buchet - August 2025
;; ===============================================================================================================

(let ( ( root (getShellEnvVar "SKILL_SHARP_ROOT") )
       )
  ;; Make sure that required sharp version or higher is loaded, try to load it otherwise
  (cond
    ( (isCallable '@sharp)              (@sharp ?min_version "0.0.0") )
    ( (and (stringp root) (isDir root)) (load (strcat root "skill/loader.scm")))
    ( t                                 (error "SKILL_SHARP_ROOT is not defined !"))
    ))

(let ()

  (@fun tcp_communication_gui ()
    ?doc "Display 'TCP Communication' GUI."
    ?global t
    ;; Display form under the mouse
    (hiDisplayForm (create_form) -1:-1)
    )

  (@fun create_form ()
    ?doc "Create the form."
    (let ( ( messages (tconc nil nil) )
           )
      (hiCreateLayoutForm (gensym 'tcp_communication_form) "TCP Communication"
        (hiCreateVerticalBoxLayout 'main_layout ?items (list
            (hiCreateHypertextField ?name 'html)
            '(stretch_item 1)
            (hiCreateComboField
              ?name 'input
              ?callback
              ;; Evaluate input in Python and display it
              (lambda (field form)
                (let ( ( msg field->value )
                       )
                  (tconc messages msg)
                  (setf form->html->value (messages_to_html messages))
                  (@queue
                    (lambda ()
                      (tconc messages (car (@bash (@str "echo '{msg}' | $SKILL_SHARP_ROOT/bin/tcp_client -l PYTHON"))))
                      (setf form->html->value (messages_to_html messages))
                      ))
                  ))
              )
            ));main_layout
        ?buttonLayout 'Empty
        ?minSize      '( 300 400 )
        );hiCreateLayoutForm
      ));let ;fun

  (@fun messages_to_html
    ( ( messages ?type list )
      )
    ?doc "Display messages as HTML"
    (buildString (cdar messages) "<br>")
    )

  );closure

;(@skill_server)
(tcp_communication_gui)

;*/

