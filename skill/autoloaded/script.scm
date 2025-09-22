#!/bin/csh -f
;; ## Header to run this file using SKILL interpreter.
; setenv SKILL_SHARP_ARGV "$argv" ; $CDS_INST_DIR/tools.lnx86/dfII/bin/skill `realpath $0` ; exit $status ;

;; ===============================================================================================================
;; Utilites to build SKILL scripts
;;
;; A. Buchet - April 2025
;; ===============================================================================================================

(setf @skill.exit_status 0)

(@fun @exit
  ( ( status ?type integer ?def @skill.exit_status )
    @rest _
    )
  ?doc    "Exits with proper exit status."
  ?out    nil
  ?global t
  (exit status)
  )


;; =======================================================
;; Functions to define script arguments
;; =======================================================

;; TODO - Add possibility to write long and short descriptions

(let ( ( description          "*UNDEFINED* (please use `@script_set_description')" )
       ( version              "???         (please use `@script_set_version')"     )
       ( arguments_by_name    (makeTable t nil)                                      )
       ( arguments_by_key     (makeTable t nil)                                      )
       ( positional_arguments (tconc nil nil)                                        )
       ( argument_count       0                                                      )
       )

  ;; -------------------------------------------------------
  ;; Script utilities
  ;; -------------------------------------------------------

  (@fun @script_get_description ()
    ?doc    "Return current script description"
    ?out    string
    ?global t
    description
    )

  (@fun @script_set_description
    ( ( str ?type string )
      )
    ?doc    "Define current script description using STR."
    ?out    string
    ?global t
    (setq description str)
    )

  (@fun @script_get_version ()
    ?doc    "Return current script version"
    ?out    string
    ?global t
    version
    )

  (@fun @script_set_version
    ( ( str ?type string )
      )
    ?doc    "Define current script version using STR."
    ?out    string
    ?global t
    (setq version str)
    )

  (@fun @script_is_running? ()
    ?doc    "Return t if current file is the running script, nil otherwise."
    ?out    t|nil
    ?global t
    ;; This is equivalent to Python (__name__ == '__main__')
    (equal (@realpath (get_filename (@piport)) )
           (@realpath (argv 0)                 )
           ))

  (@fun deduce_name
    ( ( keys ?type ( string ... ) )
      )
    ?doc "Deduce argument name from KEYS.
Remove starting dashes then return the first key still having more than one character.
Otherwise it defaults to the first key."
    ?out string
    (assert keys "@script_add_argument - At least one of ?name or ?keys is required.")
    (prog ()
      ;; Return first name with two characters, second loop returns first name
      (foreach no_length_check '( nil t )
        (foreach key keys
          (setq key (pcreReplace (pcreCompile "^-+") key "" 0))
          (and (or no_length_check (lessp 1 (length key)))
               (not (blankstrp key))
               (return key)
               )))
      (error "Unable to deduce name for keys: %A" keys)
      ));prog ;def

  ;; -------------------------------------------------------
  ;; Add script arguments
  ;; -------------------------------------------------------

  ;; TODO - Raise error when required positional argument is defined after an optional one.
  ;; TODO - Raise error when defining a positional argument after rest one.

  (@fun @script_add_argument
    ( @key
      ( keys       ?type ( string ... )|nil ?def nil                ?doc "List of keys available to use argument."               )
      ( opp_keys   ?type ( string ... )|nil ?def nil                ?doc "Alternative keys to trigger opposite behavior."        )
      ( name       ?type string             ?def (deduce_name keys) ?doc "Displayed name in usage and help."                     )
      ( doc        ?type string                                     ?doc "Argument docstring printed in help."                   )
      ( prompt     ?type string             ?def (upperCase name)   ?doc "Prompt displayed in usage when asking for a value."    )
      ( priority   ?type t|nil              ?def nil                ?doc "If non-nil, argument is set as a priority one."        )
      ( callback   ?type callable|nil       ?def nil                ?doc "Callback triggered when argument is used."             )
      ( action     ?type symbol  |nil       ?def nil                ?doc "Predefined lists of actions ."                         )
      ( type       ?type symbol  |nil       ?def nil                ?doc "Argument type, this is used for validation."           )
      ( default    ?type any                ?def '__unbound__       ?doc "Default value when argument is not provided."          )
      ( usage      ?type string  |nil       ?def nil                ?doc "Overwrite argument usage string in script help."       )
      ( hide_usage ?type t|nil              ?def nil                ?doc "If non-nil, argument usage is only displayed in help." )
      @rest _
      )
    ?doc    "Add a script argument."
    ?out    t
    ?global t
    ;; `funcall' is required so that `@script_is_running?' can be mocked later on
    (assert (funcall '@script_is_running?)
      "`@script_add_argument' can only be used in running script. (See `@script_is_running?')")
    ;; Make sure keys are valid
    (foreach keys_list (list keys opp_keys)
      (foreach key keys_list
        (assert (equal "-" (substring key 1 1)) "Argument key has to start with '-' : %N" key)
        ))
    (when opp_keys
      (assert (memq action '(store_t store_nil)) "?opp_keys can only be used with ?action amongst '(store_t store_nil)!")
      )
    ;; Check callback, action and type
    (cond
      ( (and priority (not callback))
        (error "?priority argument can only be used with ?callback argument.")
        )
      ;; Callback is provided, assert that it is the only argument
      ( callback
        (foreach ( sym val ) '( action type default ) '( nil nil __unbound__ )
          (assert (eq val (symeval sym (theEnvironment))) "%s"
            (@str "@script_add_argument - Error when defining argument \"{name}\".\n\
Both ?callback and ?{sym} cannot be provided, please remove one argument.")
            ))
        (setq action 'run_callback)
        )
      ;; Action is provided, assert that type is not defined
      ( action
        (foreach ( sym val ) '( type default ) '( nil __unbound__ )
          (assert (eq val (symeval sym (theEnvironment)))
            (@str "@script_add_argument - Error when defining argument \"{name}\".\n\
Both ?action and ?{sym} cannot be provided, please remove one argument.")
            ))
        ;; Predefined actions
        (@caseq action
          ( store_t
            (setq type    'boolean )
            (setq default nil      )
            )
          ( store_nil
            (setq type    'boolean )
            (setq default t        )
            )
          ( store_rest
            (setq type    'list    )
            (setq default nil      )
            )
          ));caseq ;action
      ( t
        ;; Default type is string
        (unless type (setq type 'string))
        (setq action 'store)
        )
      );cond
    ;; Define arguments
    (let ( ( dpl
             `( nil
                keys       ,keys
                opp_keys   ,opp_keys
                name       ,name
                doc        ,doc
                prompt     ,prompt
                priority   ,priority
                callback   ,callback
                action     ,action
                type       ,type
                usage      ,usage
                hide_usage ,hide_usage
                mandatory  ,(and (eq 'store action) (eq '__unbound__ default)) ; an argument without default value is mandatory
                default    ,default
                value      ,default
                order      ,(postincrement argument_count)
                ))
           )
      (cond
        ;; key argument
        ( keys
          (foreach keys_list (list keys opp_keys)
            (foreach key keys_list
              (setf arguments_by_key[key] dpl)
              ));foreach ;foreach
          )
        ;; positional argument
        ( t
          ;; Make sure no required argument is defined after an optional one
          (when dpl->mandatory
            (assert (forall arg_dpl (cdar positional_arguments) arg_dpl->mandatory)
              "%s" (@str "@script_add_argument - Error when defining argument \"{name}\".\n\
A positional required argument (i.e. without ?default) cannot be defined after an optional one.")
              ))
          (tconc positional_arguments dpl)
          )
        );cond
      (setf arguments_by_name[name] dpl)
      ;; Return t
      t
      );let
    );def

  ;; -------------------------------------------------------
  ;; Get and parse arguments
  ;; -------------------------------------------------------

  (@fun @script_get_arguments ()
    ?doc    "Return arguments table where elements are accessed by name."
    ?out    table
    ?global t
    arguments_by_name
    )

  (@fun get_argument_by_key
    ( ( key ?type string )
      )
    ?doc "Fetch argument associated to KEY, raise an error when not found."
    ?out dpl
    (or arguments_by_key[key]
        (error "Not a valid option: %s" key)
        ))

  (defun split_equal (str)
    "Split STR into the part before '=' and the part after, if it exists"
    (let ( (l (parseString str "=") )
           )
      (list (pop l) (when l (buildString l "=")))
      ))

  (@fun @script_parse_arguments ()
    ?doc    "Parse script arguments, set their values and trigger associated callbacks."
    ?out    list
    ?global t
    (let ( ( provided_args (parseString (getShellEnvVar "SKILL_SHARP_ARGV") " ") )
           )

      (@fun parse_argument
        ( ( arg ?type dpl    )
          ( key ?type string )
          )
        ?doc "Parse ARG, fetch value or call associated callback."
        ;; Priority argument, call it directly
        (when arg->priority
          (funcall arg->callback)
          )
        (@caseq arg->action
          ;; Store boolean value
          ( ( store_t store_nil )
            (let ( ( value (eq 'store_t arg->action) )
                   )
              (when (member key arg->opp_keys)
                (setq value (not value))
                )
              (setf arg->value value)
              ))
          ;; Store any other value
          ( store
            (setf arg->value (pop provided_args))
            )
          ;; Store remaining values
          ( store_rest
            (setf arg->value provided_args)
            )
          ));caseq ;fun

      ;; Parse script options / key arguments
      (prog ( ( pos_args (tconc nil nil) )
              provided_arg
              )
        (while (setq provided_arg (pop provided_args))
          (cond
            ;; Provided arg is double dash, next arguments are positional
            ( (equal "--" provided_arg)
              (return)
              )
            ;; Provided arg is a named option, parse it directly
            ( (equal "--" (substring provided_arg 1 2))
              (destructuringBind (key val) (split_equal provided_arg)
                (when val (push val provided_args))
                (setq provided_arg key)
                )
              (parse_argument (get_argument_by_key provided_arg) provided_arg)
              );named argument
            ;; Provided arg is a list of arguments, parse first one
            ( (equal "-" (substring provided_arg 1 1))
              (destructuringBind (key val) (split_equal provided_arg)
                (when val (push val provided_args))
                (setq provided_arg key)
                )
              (letseq  ( ( key  (substring provided_arg 1 2)       )
                         ( rest (substring provided_arg 3)         )
                         ( dpl  (get_argument_by_key key) )
                         )
                ;; Push arguments after current one on top of remaining arguments.
                (when rest
                  ;; When action is 'store, keep rest as is, otherwise add a dash
                  (unless (eq 'store dpl->action)
                    (setq rest (strcat "-" rest))
                    )
                  (push rest provided_args)
                  )
                ;; Actual parsing
                (parse_argument dpl key)
                );let
              );list of one-character arguments
            ;; Arg is a positional argument
            ( t
              (tconc pos_args provided_arg)
              )
            ));cond ;while
        ;; Add filtered positional arguments before remaining ones
        (setq provided_args (cdar (lconc pos_args provided_args)))
        );prog
      ;; Parse positional arguments
      (let ( ( pos_args (copy (cdar positional_arguments)) )
             )
        ;; When last argument is rest, make an infinite loop so it matches any number of inputs
        (letseq ( ( dpl           (car (last pos_args)) )
                  ( infinite_loop (list dpl)            )
                  )
          (when (eq 'store_rest dpl->action)
            (nconc pos_args infinite_loop infinite_loop)
            ));when ;let
        ;; Browse required and provided arguments
        (while provided_args
          ;; Raise an error if too many arguments were provided
          (unless pos_args
            (fprintf (@errport) "%s" (@str "Too many positional arguments were provided!\n\n"))
            (_\@script_show_usage)
            )
          ;; Browse defined and provided arguments together
          (let ( ( defined_arg  (pop pos_args      ) )
                 ( provided_arg (pop provided_args ) )
                 )
            (if (eq 'store_rest defined_arg->action)
                (setf defined_arg->value (cons provided_arg defined_arg->value))
              (setf defined_arg->value provided_arg)
              ));if ;let
          );while
        ;; Raise an error if too few arguments were provided
        (when (car pos_args)->mandatory
          (fprintf (@errport) "%s" (@str "More positional arguments are required!\n\n"))
          (_\@script_show_usage)
          )
        ;; Re-order rest args
        (when (and (car pos_args) (eq 'store_rest (car pos_args)->action))
          (setf (car pos_args)->value (reverse (car pos_args)->value))
          )
        );let
      ;; Raise errors for missing required arguments.
      (let ( ( mandatory_args (setof arg (@table_elements (@script_get_arguments)) arg->mandatory) )
             )
        (foreach arg (@sort mandatory_args ?shape (@getter 'order) ?comp 'lessp)
          (when (eq '__unbound__ arg->value)
            (fprintf (@errport) "%s" (@str "Argument '{arg->name}' is required!\n\n"))
            (_\@script_show_usage)
            ));when ;foreach
        );let
      ));let ;fun

  );closure

;; Add `setf' helpers
(define setf_\@script_get_description (getd '@script_set_description))
(define setf_\@script_get_version     (getd '@script_set_version    ))

;; Fix lint warnings
(define setf_\\\@script_get_description (getd '@script_set_description))
(define setf_\\\@script_get_version     (getd '@script_set_version    ))

(setf (fdoc 'setf_\@script_get_description  ) "`setf' helper for `@script_get_description'")
(setf (fdoc 'setf_\\\@script_get_description) "`setf' helper for `@script_get_description'")

(setf (fdoc 'setf_\@script_get_version      ) "`setf' helper for `@script_get_version'"    )
(setf (fdoc 'setf_\\\@script_get_version    ) "`setf' helper for `@script_get_version'"    )


;; =======================================================
;; Define default arguments
;; =======================================================

(let ()

  ;; -------------------------------------------------------
  ;; Help, usage & version callbacks
  ;; -------------------------------------------------------

  (@fun get_arguments_by_type ()
    ?doc "Return sorted list of arguments"
    ?out ( list list list list list )
    (let ( callback_arguments
           optional_key_arguments
           required_key_arguments
           positional_arguments
           optional_arguments
           rest_arguments
           )
      ;; Browse elements, sort them into lists according to their type
      (foreach elt (@table_elements (@script_get_arguments))
        (cond
          ( elt->callback                  (push elt callback_arguments    ) )
          ( (eq 'store_rest elt->action)   (push elt rest_arguments        ) )
          ( (and elt->keys elt->mandatory) (push elt required_key_arguments) )
          ( elt->keys                      (push elt optional_key_arguments) )
          ( elt->mandatory                 (push elt positional_arguments  ) )
          ( t                              (push elt optional_arguments    ) )
          ));cond ;foreach
      (nconc
        ;; Key arguments are sorted by name
        (mapcar (lambda (l) (@sort l ?shape (@getter 'name) ?comp '@alphalessp))
          (list
            callback_arguments
            optional_key_arguments
            required_key_arguments
            ))
        ;; Postional arguments are sorted by order
        (mapcar (lambda (l) (@sort l ?shape (@getter 'order) ?comp 'lessp))
          (list
            positional_arguments
            optional_arguments
            rest_arguments
            ))
        );nconc
      ));let ;fun

  ;; -------------------------------------
  ;; Usage
  ;; -------------------------------------

  (@fun argument_usage
    ( ( arg ?type dpl )
      )
    ?doc "Return ARG usage string."
    ?out string
    (or arg->usage
        (setf arg->usage
          (let ( ( str (buildString (progn "NO_LINT" (append arg->keys arg->opp_keys)) "|") )
                 )
            (caseq arg->action
              ( store      (setq str (lsprintf "%s%s%s" str (if arg->keys "=" "") arg->prompt)) )
              ( store_rest (setq str (lsprintf "%s..." arg->prompt                           )) )
              )
            (unless arg->mandatory        (setq str (lsprintf "[%s]" str)))
            str));let ;setf
        ));or ;fun

  (@fun arguments_usage ()
    ?doc "Return arguments usage string."
    ?out string
    (let ( ( arguments (apply 'nconc (get_arguments_by_type)) )
           )
      (buildString (mapcar argument_usage (setof arg arguments (not arg->hide_usage))))
      ));let ;fun

  (@fun print_usage ()
    ?doc "Print script usage"
    ?out nil
    ;; TODO - Print positional arguments in usage string
    (fprintf (@errport) (@str "\
Usage: {(@basename (argv 0))} {(arguments_usage)}
{(@script_get_description)}
"))
    nil
    )

  ;; -------------------------------------
  ;; Help
  ;; -------------------------------------

  (@fun argument_help
    ( ( arg           ?type dpl     )
      ( max_usage_len ?type integer )
      ( max_help_len  ?type integer )
      )
    ?doc "Return ARG help string."
    ?out string
    (let ( ( lines       (parseString arg->doc "\n")                                                           )
           ( has_default (and (not arg->mandatory) (memq arg->action '( store store_t store_nil store_rest ))) )
           )
      (strcat
        ;; Print usage with two spaces
        (@padd arg->usage (plus 2 max_usage_len))  " " " "
        ;; Print padded first help line with default value
        (if (not has_default) (or (car lines) "")
          (strcat
            (@padd (or (car lines) "") (minus max_help_len))
            (lsprintf " [default: %N]" arg->default)
            ))
        ;; Print other help lines with identation
        (buildString (cons "" (cdr lines)) (@padd "\n" (minus (plus max_usage_len 5))))
        );strcat
      ));let ;fun

  (@fun arguments_help
    ( ( args          ?type list    )
      ( max_usage_len ?type integer )
      ( max_help_len  ?type integer )
      )
    ?doc "Return ARGS help multiline string."
    ?out string
    (buildString (foreach mapcar arg args (argument_help arg max_usage_len max_help_len)) "\n")
    )

  (@fun print_help ()
    ?doc "Print script help."
    ?out nil
    (let ( ( arguments_lists (get_arguments_by_type) )
           ( max_usage_len   0                       )
           ( max_help_len    0                       )
           )
      ;; Fetch maximum usage and help length
      (foreach arguments arguments_lists
        (foreach arg arguments
          (setq max_usage_len (max max_usage_len (length (argument_usage arg))))
          (setq max_help_len  (apply 'max max_help_len (mapcar 'length (parseString arg->doc "\n"))))
          ))
      (destructuringBind ( callback_arguments
                           optional_key_arguments
                           required_key_arguments
                           positional_arguments
                           optional_arguments
                           rest_arguments
                           )
                         arguments_lists
        (fprintf (@errport) "%s" (@str "\

Arguments
{(arguments_help required_key_arguments max_usage_len max_help_len)}
{(arguments_help positional_arguments   max_usage_len max_help_len)}
{(arguments_help optional_arguments     max_usage_len max_help_len)}
{(arguments_help rest_arguments         max_usage_len max_help_len)}

Options
{(arguments_help optional_key_arguments max_usage_len max_help_len)}

Callbacks
{(arguments_help callback_arguments     max_usage_len max_help_len)}
"))
        )
        ;; TODO - Print arguments and arguments with doc
        nil
    ))

  (@fun _\@script_show_help ()
    ?doc    "Show script help and exit."
    ?out    nil
    ?global t
    (print_usage)
    (print_help )
    (exit 1)
    )

  (@fun _\@script_show_usage ()
    ?doc    "Show script usage and exit."
    ?out    nil
    ?global t
    (print_usage)
    (exit 1)
    )

  (@fun _\@script_show_version ()
    ?doc    "Show script version and exit."
    ?out    nil
    ?global t
    ;; TODO - write version function
    (printf (@str "{(@basename (argv 0))} {(@script_get_version)}\n"))
    (exit 0)
    )

  ;; -------------------------------------------------------
  ;; Add default arguments (help, usage, version)
  ;; -------------------------------------------------------

  ;; Mock `@script_is_running?' to define priority arguments in a different file than script one
  (@letf ( ( (getd '@script_is_running?) (lambda _ t) )
             )

    (@script_add_argument
      ?keys       '( "-h" "--h" "--help" )
      ?doc        "Show script help and exit."
      ?callback   '_\@script_show_help
      ?priority   t
      ?usage      "[-h|--help]"
      ?hide_usage t
      )

    (@script_add_argument
      ?keys       '( "-u" "--u" "--usage" )
      ?doc        "Show script usage and exit."
      ?callback   '_\@script_show_usage
      ?priority   t
      ?usage      "[-u|--usage]"
      ?hide_usage t
      )

    (@script_add_argument
      ?keys       '( "-v" "--v" "--version" )
      ?doc        "Show script version and exit."
      ?callback   '_\@script_show_version
      ?priority   t
      ?usage      "[-v|--version]"
      ?hide_usage t
      )

    );letf

  );closure

;*/

