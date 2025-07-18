;; Lint messages regarding Python f-strings

;; TODO - No idea why but if this call is removed, Lint does not expand first MACROEXP1 error...
(@str "Dummy")

;; Raise an error and a Lint error: `@str' argument has to be a string, not a variable.
(let ( ( str "test {nil}" )
       )
  (@str str)
  )

;; Raise an error and a Lint error: too many arguments are provided
(@str "{12+27} %N" 'test)

;; Raise an error and a Lint error: open bracket is never closed.
(@str "Bracket is never closed { 12 27")

;; Raise a warning: closing bracket is never opened
(@str "Bracket is never opened } 12 27")

;; This should not raise any message
(let ( ( var 12 )
       )
  (@str "{var} {var}")
  )


