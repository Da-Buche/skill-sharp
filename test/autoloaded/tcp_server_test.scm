(@test
  ?fun '@skill_server
  ?doc "Make sure server is working"
  ?skip t ;(not (member (@basename (car (getShellArgs))) '( "cdsmps" "virtuoso" )))

  ;; TODO - @skill_server test is not robust enough
  (@assertion
    (progn
      (setf @skill_server.test_server (@skill_server) )
      (ipcWaitForProcess @skill_server.test_server)
      (@bash "echo '(progn (println 12) (ipcKillProcess @skill_server.test_server)' | $SKILL_SHARP_ROOT/bin/tcp_client -l skill")
      (ipcWait @skill_server.test_server 30 5)
      )
    ?out t
    )

  )
