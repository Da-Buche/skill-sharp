;; ===============================================================================================================
;; Macro to define macros, this is meta !
;;
;; A. Buchet - July 2025
;; ===============================================================================================================

(let ( ( identifiers
         ;; From SKILL documentation 'Identifiers Used to Denote Data Types'
         '( ( "a"  array                 "array"                                                                                    )
            ( "A"  amsobject             "AMS object"                                                                               )
            ( "b"  ddUserType            "DDPI object"                                                                              )
            ( "B"  ddCatUserType         "DDPI category object"                                                                     )
            ( "C"  opfcontext            "OPF context"                                                                              )
            ( "d"  dbobject              "Cadence database object (CDBA)"                                                           )
            ( "e"  envobj                "environment"                                                                              )
            ( "f"  flonum                "floating-point number"                                                                    )
            ( "F"  opffile               "OPF file ID"                                                                              )
            ( "g"  general               "any data type"                                                                            )
            ( "G"  gdmSpecIlUserType     "generic design management (GDM) spec object"                                              )
            ( "h"  hdbobject             "hierarchical database configuration object"                                               )
            ( "I"  dbgenobject           "CDB generator object"                                                                     )
            ( "K"  mapiobject            "MAPI object"                                                                              )
            ( "l"  list                  "linked list"                                                                              )
            ( "L"  tc                    "Technology file time stamp"                                                               )
            ( "m"  nmpIlUserType         "nmpIl user type"                                                                          )
            ( "M"  cdsEvalObject         "cdsEvalObject"                                                                            )
            ( "n"  number                "integer or floating-point number"                                                         )
            ( "o"  userType              "user-defined type (other)"                                                                )
            ( "p"  port                  "I/O port"                                                                                 )
            ( "q"  gdmspecListIlUserType "gdm spec list"                                                                            )
            ( "r"  defstruct             "defstruct"                                                                                )
            ( "R"  rodObj                "relative object design (ROD) object"                                                      )
            ( "s"  symbol                "symbol"                                                                                   )
            ( "S"  stringSymbol          "symbol or character string"                                                               )
            ( "t"  string                "character string (text)"                                                                  )
            ( "T"  txobject              "transient object"                                                                         )
            ( "u"  function              "function object, either the name of a function (symbol) or a lambda function body (list)" )
            ( "U"  funobj                "function object"                                                                          )
            ( "v"  hdbpath               "hdbpath"                                                                                  )
            ( "w"  wtype                 "window type"                                                                              )
            ;( "sw" swtype                "subtype session window"                                                                   )
            ;( "dw" dwtype                "subtype dockable window"                                                                  )
            ( "x"  integer               "integer number"                                                                           )
            ( "y"  binary                "binary function"                                                                          )
            ;( "&"  pointer               "pointer type"                                                                             )
            ))
       )

  (defun get_name (fun "u")
    "Return FUN name even if FUN is a lambda object."
    (if (symbolp fun) fun
      (let ( ( fun_name (lsprintf "%N" fun) )
             ( magic    (rexMagic)          )
             )
        (rexMagic t)
        (unwindProtect
          (when (pcreMatchp "funobj:(.+)" fun_name)
            (concat (pcreSubstitute "\\1"))
            )
          (rexMagic magic)
          ))
      ));if ;def

  ;; -------------------------------------
  ;; Properly store arglist
  ;; -------------------------------------

  (defglobalfun _\@arglist_expand (args)
    "Expand argument list ARGS to match `@fun' format."
    (let ( ( val_types (and (stringp (car (last args)))
                            (foreach mapcar letter (parseString (popf (last args)) "")
                              (cadr (assoc letter identifiers))
                              )))
           ( arg_type  '@positional )
           )
      ;; Build infinite types list (as last letter matches all remaining arguments)
      (when val_types (nconc val_types (last val_types)))
      ;; Browse arg and types together
      (foreach mapcan arg args
        (caseq arg
          ( ( @optional       ) (setq arg_type arg ) nil        )
          ( ( @key @rest @aux ) (setq arg_type arg ) (list arg) )
          ( t
            ;; `list' here compensates for mapcan
            (list
              (letseq ( ( val_type (pop val_types)                        )
                        ( res      (when val_type (list '?type val_type)) )
                        )
                (caseq arg_type
                  ;; Argument has no default value
                  ( ( @positional ) (cons arg res) )
                  ;; Argument has default value, check if it is provided, default to nil otherwise
                  ( ( @optional @key @rest @aux )
                    (caseq (type arg)
                      ( symbol (constar arg       '?def nil        res)                       )
                      ( list   (constar (car arg) '?def (cadr arg) res)                       )
                      ( t      (error "caseq - (type arg) should be one of '( symbol list )") )
                      ))
                  ( t (error "caseq - arg_type should be one of '( @positional @optional @key @rest @aux )") )
                  ));caseq ;letseq
              ));list ;t
          ));caseq ;foreach mapcan
      ));let ;def

  (defglobalfun @arglist ( fun "u" )
    "Return FUN arguments list."
    ;; TODO - @arglist should work with macros
    (or (get (get_name fun) '@arglist)
        (_\@arglist_expand (arglist fun))
        ));or ;fun

  (defglobalfun setf_\@arglist ( args fun "ls" )
    "Set ARGS as FUN arguments list."
    ;; TODO - setf_\@arglist should work with macros
    (setf (get fun '@arglist) args)
    )

  ;; -------------------------------------
  ;; Properly store fdoc
  ;; (otherwise `fdoc' gets overwritten)
  ;; -------------------------------------

  (defglobalfun @fdoc ( fun "u" )
    "Return FUN arguments list."
    ;; TODO - @fdoc should work with macros
    (let ( (name (get_name fun))
           )
      (when (symbolp name)
        (or (get name '@fdoc)
            (get name '_fdoc)
            (fdoc name)
            ))
      ));let ;def

  (defglobalfun setf_\@fdoc ( args fun "ts" )
    "Set ARGS as FUN arguments list."
    ;; TODO - setf_\@fdoc should work with macros
    (setf (get fun '@fdoc) args)
    (setf (get fun '_fdoc) args)
    )

  ;; -------------------------------------
  ;; Properly store function output
  ;; (otherwise `fdoc' gets overwritten)
  ;; -------------------------------------

  (defglobalfun @output ( fun "u" )
    "Return FUN arguments list."
    ;; TODO - @output should work with macros
    (let ( (name (get_name fun))
           )
      (when (symbolp name) (get name '@output))
      ));let ;def

  (defglobalfun setf_\@output ( args fun "gs" )
    "Set ARGS as FUN arguments list."
    ;; TODO - setf_\@output should work with macros
    (setf (get fun '@output) args)
    )

  );closure

;; TODO - Lint rule SETF3 does not work with quoted characters
;; The comparison works with double escape (contact Cadence support)
(define setf_\\\@arglist (getd 'setf_\@arglist))
(define setf_\\\@fdoc    (getd 'setf_\@fdoc   ))
(define setf_\\\@output  (getd 'setf_\@output ))

(setf (fdoc 'setf_\\\@arglist) "`setf' helper for `@arglist'")
(setf (fdoc 'setf_\\\@fdoc   ) "`setf' helper for `@fdoc'"   )
(setf (fdoc 'setf_\\\@output ) "`setf' helper for `@output'" )


(setf (@fdoc '@macro) "`defmacro' wrapper, mostly for documentation purposes.")

(defmacro @macro ( name args doc @rest body )
  ;; Check inputs before their evaluation
  (assert (symbolp name) "@macro - %s argument %s should be a %s: %N" 'first  'NAME 'symbol doc)
  (assert (listp   args) "@macro - %s argument %s should be a %s: %N" 'second 'ARGS 'list   doc)
  (assert (stringp doc ) "@macro - %s argument %s should be a %s: %N" 'third  'DOC  'string doc)
  ;; Build call to `setf_fdoc' and `defmacro'
  (list 'prog1
    (constar 'defmacro name args body)
    `(setf (fdoc     ',name) ,doc                      )
    `(setf (@arglist ',name) (_\@arglist_expand ',args))
    ))

;*/

