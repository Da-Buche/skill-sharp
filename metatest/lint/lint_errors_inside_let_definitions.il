
(let ( a
       ( b
         (car
           (setof num (list 1 2 3) (evenp num))
           )
         )

       ( c
         (setq a 12 27)
         )

       )

  (list a b c)
  )


(letseq ( ( var0
            (setq toto 12 27)
            )
          ( var1 abc )
          ( var2
            (car
              (setof num (list 1 2 3) (evenp num))
              )
            )
          )

  (list var0 var1 var2)
  )

