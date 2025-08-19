;; ===============================================================================================================
;; Server to evaluate incoming data and return SKILL++ evaluation results.
;;
;; A. Buchet - August 2025
;; ===============================================================================================================

(@fun @skill_server
  ( @key
    ( verbose ?def nil ?doc "If non-nil, print a message whenever the server receives a connection." )
    ( restart ?def nil ?doc "If non-nil, server is automatically restarted when it ends."            )
    @rest _ )
  ?doc "Start SKILL++ server, it opens an available port and evaluates all incoming data as SKILL++ and return it through the same port."
  (ipcBeginProcess
    (let ( ( script      (@realpath "$SKILL_SHARP_ROOT/bin/tcp_server") )
           ( verbose_arg (if verbose "-v" "")                           )
           )
      (@str "{script} -l SKILL++ {verbose_arg}")
      );let
    ""
    ;; Evaluate any incoming data from pid
    (lambda ( pid data )
      (let ( ( res (errsetstring (lsprintf "(progn %s)" data) nil 'ils) )
             )
        (ipcWriteProcess pid (lsprintf "%N" res))
        (unless res
          (@warn "Error when reading from {pid}:\n\
DATA: |{data}|\n\
ERROR: {errset.errset}")
          (warn "")
          (getWarn)
          )
        ))
    ;; Print errors
    (lambda ( pid data ) (@info "{pid} message: {data}"))
    ;; Restart server when necessary
    (if restart (lambda ( _pid _status ) (@skill_server ?restart t) @nil))
    ))

;*/

