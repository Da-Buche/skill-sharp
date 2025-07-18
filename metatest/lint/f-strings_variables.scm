;; unused_var should be reported as unused
;;
;; This test case asserts everything works fine for `@fun' and `@fprintf'
;; (also implying other `@str' like macros)

(let ( ( used_variable "black"     )
       ( second_color  "darkgreen" )
       ( unused_var    "darkblue"  )
       )

  (@fun colors_example ()
    ?doc    "Print example."
    ?global t
    (@fprintf (@poport) "{used_variable} {second_color}\n")
    )

  )

