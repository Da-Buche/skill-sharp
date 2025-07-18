

;; Those are valid `status' and `sstatus' calls
(status profCount        )
(status verboseLoad      )
(status verboseNamespace )

(sstatus profCount        nil)
(sstatus verboseLoad      t  )
(sstatus verboseNamespace t  )

;; The following does not exist though
(status  unknown_status_var    )
(sstatus thisDoesNotExist   nil)

