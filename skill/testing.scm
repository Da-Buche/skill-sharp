;; ===============================================================================================================
;; Unit-test framework
;;
;; A. Buchet - June 2025
;; ===============================================================================================================

;; =======================================================
;; Test class
;; =======================================================

(@class
  ?name    '@test
  ?doc     "Class to store unit-tests, associated assertions and results."
  ?builder nil
  ( fun         @arg @err                                                                                                                )
  ( title       @arg @err                                                                                                                )
  ( doc         @arg @err                                                                                                                )
  ( body        @arg @err                                                                                                                )
  ( file        ?init (get_filename piport)           ?doc "File in which test is defined, very useful when debugging failed tests."     )
  ( assertions  ?init (tconc nil nil)                                                                                                    )
  ( environment ?init nil                             ?doc "Scheme environment (if any) in which test is defined (and supposed to run)." )
  ( status      ?init (@set_status (@built_obj) 'new) ?doc "Current test status, it should be amongst '( new skip pass fail )"           )
  ( messages    ?init nil                                                                                                                )
  )

(defmethod printself ( ( obj @test ) )
  "Prettifier for @test class."
  (lsprintf "@test(%N)[%N]" obj->title obj)
  )

(defmethod @get_assertions ( (test @test) )
  "Get TEST assertions objects."
  (cdar test->assertions)
  )

;; Store and access defined tests
(let ( ( tests (tconc nil nil) )
       )

  (@fun get_fun_type
    ( ( test ?type @test  )
      ( name ?type symbol )
      )
    ?doc "Return type of function named NAME."
    ?out symbol
    (let ( ( fun_obj (or (car (errset (symeval name test->environment))) name) )
           )
      (or (car (errset (getFunType fun_obj))) '???)
      ));let ;fun

  (defmethod initializeInstance @after ( ( test @test ) @rest _ )
    "Set OBJ as currently built one, so it can be accessed in class default values."
    ;; Update test title when blank
    (unless (@nonblankstring? test->title) (setf test->title (@str "{(get_fun_type test test->fun)} {test->fun}")))
    ;; Properly stack test
    (tconc tests test)
    );method

  (@fun _\@tests ()
    ?doc    "Return all defined tests."
    ?out    ( @test ... )
    ?global t
    (cdar tests)
    )

  (@fun _\@tests_reset ()
    ?doc    "Reset all defined tests"
    ?out    t
    ?global t
    (setq tests (tconc nil nil))
    (_\@tests_reset_counters)
    t)

  );closure


;; =======================================================
;; Assertion class
;; =======================================================

;; TODO - @class macro should be improved so _\@assertion builder is properly defined by default (using ?builder '_\@assertion_)

(@class
  ?name    '@assertion
  ?doc     "Class to store unit-tests, associated assertions and results."
  ?builder nil
  ;; TODO - Check that class builder arglist is well defined
  ( num           @arg @err                                                                                                           )
  ( doc           @arg @err                                                                                                           )
  ( status        ?init (@set_status (@built_obj) 'new) ?doc "Current assertion status, it should be amongst '( new skip pass fail )" )
  ( messages      ?init nil                                                                                                           )
  ( body_quoted                                                                                                                       )
  ( body_expected                                                                                                                     )
  ( body_result                                                                                                                       )
  ( info_quoted                                                                                                                       )
  ( info_expected                                                                                                                     )
  ( info_result                                                                                                                       )
  ( warn_quoted                                                                                                                       )
  ( warn_expected                                                                                                                     )
  ( warn_result                                                                                                                       )
  ( error_quoted                                                                                                                      )
  ( error_expected                                                                                                                    )
  ( error_result                                                                                                                      )
  )

(defmethod printself ( ( obj @assertion ) )
  "Prettifier for @test class."
  (lsprintf "@assertion(%N)[%N]" obj->num obj))

;; TODO - Remove commented code
; (defmethod initializeInstance @after ( ( assertion @assertion ) @rest _ )
;   "Set OBJ as currently built one, so it can be accessed in class default values."
;   (assert (@xor (neq assertion->out '__undefined__) assertion->error)
;     "@assertion - Exactly one of ?out or ?error should be provided.")
;   )

;; =======================================================
;; Set status and count objects
;; =======================================================

(let ( ( test_new       0 )
       ( test_skip      0 )
       ( test_pass      0 )
       ( test_fail      0 )
       ( assertion_new  0 )
       ( assertion_skip 0 )
       ( assertion_pass 0 )
       ( assertion_fail 0 )
       )

  (@fun _\@tests_reset_counters ( @rest _ )
    ?doc    "Reset all test counters"
    ?out    t
    ?global t
    (setq test_new       0)
    (setq test_skip      0)
    (setq test_pass      0)
    (setq test_fail      0)
    (setq assertion_new  0)
    (setq assertion_skip 0)
    (setq assertion_pass 0)
    (setq assertion_fail 0)
    t)

  (defmethod @set_status ( ( test @test ) value @rest _ )
    "Set TEST status to VALUE. Increase counters accordingly"
    (@caseq value
      ( new  test_new++  )
      ( skip test_skip++ )
      ( pass test_pass++ )
      ( fail test_fail++ )
      )
    (setf test->status value)
    )

  (defmethod @set_status ( ( assertion @assertion ) value @rest _ )
    "Set ASSERTION status to VALUE. Increase counters accordingly"
    (@caseq value
      ( new  assertion_new++  )
      ( skip assertion_skip++ )
      ( pass assertion_pass++ )
      ( fail assertion_fail++ )
      )
    (setf assertion->status value)
    )

  (@fun @test_print_report ( @key ( globals ?type ( symbol ... )|nil ?def nil ) @rest _)
    ?doc    "Print Unit-Tests report."
    ?out    t|nil
    ?global t
    ;; Print failure messages
    (foreach test (_\@tests)
      (when (eq 'fail test->status)
        (let ( ( port (@errport) )
               )
          (@fprintf port "Failed {test%A} from {test->file}\n\n")
          ;; Print test messages
          (foreach msg test->messages
            (@fprintf port "{msg}\n\n")
            )
          ;; Print assertions messages
          (foreach assertion (@get_assertions test)
            (when assertion->messages
              (let ( ( prettified_body (buildString (foreach mapcar elt assertion->body_quoted (@pretty_print elt)) "\n  ") )
                     )
                (@fprintf port "Failures when running {assertion%A}:\n\
  {prettified_body}\n\n")
                ))
            (foreach msg assertion->messages
              (@fprintf port "{msg}\n\n")
              ))
          (newline port)
          (drain port)
          ))
      )
    ;; Print untested functions
    (letseq ( ( untested_names (sort (setof name globals (not (get name '@test))) 'alphalessp)                     )
              ( pass           (and (plusp test_new) (zerop test_fail) (zerop assertion_fail) (not untested_names)) )
              )
      ;; Print untested functions
      (when untested_names
        (@info "Untested Functions:\n  {(buildString untested_names \"\n  \")}\n")
        (newline)
        )
      ;; Print global report
      (@info "\
Total tests: {test_new}\n\
 - skipped tests: {test_skip}\n\
 - passed  tests: {test_pass}\n\
 - failed  tests: {test_fail}\n\
\n\
Total assertions: {assertion_new}\n\
 - skipped assertions: {assertion_skip}\n\
 - passed  assertions: {assertion_pass}\n\
 - failed  assertions: {assertion_fail}\n\
\n\
{(if pass 'PASS 'FAIL)}
")
      ;; Return t when all tests passed, nil otherwise
      pass
      ));let ;fun

  );closure


;; -------------------------------------------------------
;; Public `@test' and `@assertion' macros
;; -------------------------------------------------------

(@fun _\@test_update_body
  ( ( test ?type @test )
    )
  ?doc "Parse TEST body, generates and store assertions accordingly."

  (let ( ( num 0 )
         )

    (defun parse_body (sexp)
      "Replace `@assertion' calls inside SEXP."
      (cond
        ;; sexp is an atom, return it as is
        ( (atom sexp) sexp )
        ;; sexp is a list looking like an `@assertion' call
        ;; Define assertion, store it inside test and return expression to run it.
        ( (eq '@assertion (car sexp))
          (destructuringBind ( @key (doc "") (out '__undefined__) (error '__undefined__) @rest _ ) (cdr sexp)
            ;; Assert cannot be used here as `error' is redefined locally
            (unless (stringp doc) (funcall 'error "@assertion - ?doc should be a string."))
            (unless (@xor (eq out '__undefined__) (eq error '__undefined__))
              (funcall 'error "@assertion - Exactly one of ?out or ?error should be provided."))
            ;; Define assertion and expand it in `_@assertion' macro call
            (let ( ( assertion (makeInstance '@assertion ?num num++ ?doc doc) ) )
              (tconc test->assertions assertion)
              `(_\@assertion ,assertion ,@(cdr sexp))
              ));let ;dbind
          )
        ;; sexp is any other list
        (t (mapcar parse_body sexp))
        ))

    ;; Parse body to find assertions
    (setf test->body (parse_body test->body))
    ;; Make sure test or all assertions are documented
    (assert (@get_assertions test) "%A - test has no assertion." test)
    (assert (or (@nonblankstring? test->doc)
                (forall assertion (@get_assertions test) (@nonblankstring? assertion->doc))
                )
      "%A - either test or all assertions should be documented using ?doc argument." test)
    ));let ;fun

(@macro @test ( @key
                ( fun     (@error "@test - ?fun is required, it should be a quoted symbol. (It can be set to 'nofun)") )
                ( inherit nil                                                                                          )
                ( title   ""                                                                                           )
                ( doc     ""                                                                                           )
                ( skip    nil                                                                                          )
                @rest body )
  "TODO - `@test' macro is neither finished nor properly documented."
  ;; Check input arguments
  (assert (and (listp fun)
               (eq 'quote (car fun))
               (symbolp (cadr fun))
               (not (cddr fun))
               )
    "@test - ?fun is required, it should be a quoted symbol. (It can be set to 'nofun)")
  (assert (or (nequal fun ''nofun) (@nonblankstring? title)) "@test - ?title is required and should be a non-blank string when ?fun is 'nofun.")
  (assert (or (not inherit) (and (listp inherit) (eq 'quote (car inherit)) (symbolp (cadr inherit)) (not (cddr inherit))))
    "@test - ?inherit should be a quoted symbol (or nil).")
  (assert (stringp doc  ) "@test - ?doc should be a string.")
  (assert (stringp title) "@test - ?title should be a string.")
  (@if inherit
       ;; Test is inherited
       `(setf (get ',(cadr fun) '@test) (get ',(cadr inherit) '@test))
    ;; Test is normally defined
    (@when (car (exists arg body (and (symbolp arg) (equal "?" (substring arg 1 1)))))
      ?var key_arg
      (error "@test - Unrecognized key argument: %N" key_arg)
      )
    ;; Define test
    ;; It might be a good idea to define a SHELL variable (like $SKILL_SHARP_NO_TEST) to completely skip test definitions
    ;; (This would be cleaner for production code or when creating a context)
    (when (equal "TRUE" (getShellEnvVar "SKILL_SHARP_RUN_TEST"))
      (let ( ( test (makeInstance '@test ?fun (cadr fun) ?title title ?doc doc ?body body ?skip skip) )
             )
        ;; Parse and update test body to find assertions
        (_\@test_update_body test)
        (setq body test->body)
        ;; Return updated body
        `(progn
           ;; For global functions, store test behind the function name symbol
           (when (getd (get ,test 'fun))
             (when (@get ,test 'fun '@test)
               (@set_status ,test 'fail)
               (let ( ( msg (lsprintf "Function %s is already tested by %A."
                              (get ,test 'fun) (@get ,test 'fun '@test)) )
                      )
                 (pushf msg (get ,test 'messages))
                 (error "%s" msg)
                 ))
             (setf (@get ,test 'fun '@test) ,test)
             )
           ;; Store test environment to be able to re-run it
           (setf (get ,test 'environment) (theEnvironment))
           (cond
             ;; Check skip boolean, skip and mark test accordingly
             ( ,skip
               (@set_status ,test 'skip)
               (foreach assertion (@get_assertions ,test)
                 (@set_status assertion 'skip)
                 )
               )
             ;; Run test, mark it as ran
             ;; TODO - Maybe put a switch or a variable here to detail errors in test (or not)
             ;; For now showing all errors seems simpler for debugging
             ( (errset (progn ,@body) t)
               (@update_status ,test)
               )
             ;; Error occured, mark test as failed
             ( t
               (pushf
                 (lsprintf "Error occured when running test: %N\n" (nth 4 errset.errset))
                 (get ,test 'messages)
                 )
               (@set_status ,test 'fail)
               )
             );cond
           );progn
         ));let ;when
    ));if ;macro

(@macro _\@assertion (assertion @key doc skip info warn error (out ''__undefined__) @rest body )
  "Actual assertion builder, store values and run assertion checks."
  ;; Lint waiver
  doc
  ;; Check input arguments
  (unless (classp assertion '@assertion)
    (funcall 'error "_\\@assertion - First argument should be an @assertion object.")
    )
  (@when (car (exists arg body (and (symbolp arg) (equal "?" (substring arg 1 1)))))
    ?var key_arg
    (funcall 'error "@assertion - Unrecognized key argument: %N" key_arg)
    )

  ;; A. Buchet - 2025/07/03
  ;; Here we define assertion with quoted values which will be printed in case of failure.
  ;; But actual code will be run directly after body is expanded by `@test' macro.
  ;; (Not sure this a good idea but this avoid using `eval' and keep test and assertion body
  ;; inside current environment)

  ;; Store quoted values inside assertion so it can be re-run
  (setf assertion->body_quoted  body )
  (setf assertion->info_quoted  info )
  (setf assertion->warn_quoted  warn )
  (setf assertion->error_quoted error)
  `(progn
     ;; Update expected values
     (setf (get ,assertion 'body_expected ) ,out  )
     (setf (get ,assertion 'info_expected ) ,info )
     (setf (get ,assertion 'warn_expected ) ,warn )
     (setf (get ,assertion 'error_expected) ,error)
     (@if ,skip
          (@set_status ,assertion 'skip)
       ;; Redirect standard ports
       (@with ( ( __info_port__  (outstring) )
                ( __warn_port__  (outstring) )
                ( __error_port__ (outstring) )
                )
         (@letf ( ( (@poport )             __info_port__  )
                  ( (@woport )             __warn_port__  )
                  ( (@errport)             __error_port__ )
                  ( (status forceWarnings) t              )
                  )
           ;; Run assertion, store results
           (setf (get ,assertion 'body_result ) (errset (progn nil ,@body) t) )
           (setf (get ,assertion 'info_result ) (getOutstring __info_port__ ) )
           (setf (get ,assertion 'warn_result ) (getOutstring __warn_port__ ) )
           (setf (get ,assertion 'error_result) (getOutstring __error_port__) )
           ;; Update assertion
           (@update_status ,assertion)
           ));letf ;with
       );if
     ));progn ;macro

;; This macro exists for documentation purposes, it is only called when user misuses it
(@macro @assertion ( @key doc skip info warn error out @rest body )
  "Make sure `@assertion' is never used outside `@test'."
  ;; Lint waivers
  doc skip info warn error out body
  '(error "%s" "`@assertion' should never be called outside `@test'.")
  );macro


;; =======================================================
;; Update tests/assertions statuses according to results
;; =======================================================

(defmethod @update_status ( ( assertion @assertion ) @rest _ )
  "Update ASSERTION status according to expectations and results stored in properties."

  (defun fail (msg)
    "Fail assertion with MSG"
    (pushf msg assertion->messages)
    (unless (eq 'fail assertion->status) (@set_status assertion 'fail))
    )

  (defun match_messages (expectation result)
    "Return t if RESULT matches EXPECTATION, nil otherwise."
    (or (equal      expectation result     )
        (index      result      expectation)
        ;(rexMatchp  expectation result     )
        (pcreMatchp expectation result     )
        ))

  (@nif assertion->body_result
          ;; Assertion body failed, compare error messages with expectations
          (cond
            ( (not assertion->error_expected)
              (fail (@str "Unexpected error:\n\
Got     :█{assertion->error_result}█"))
              )
            ( (not (match_messages assertion->error_expected assertion->error_result))
              (fail (@str "Different error message:\n\
Expected: █{assertion->error_expected}█\n\
Got     : █{assertion->error_result  }█"))
              )
            );cond
      ;; Compare result with expectations
      (cond
        ( assertion->error_expected
          (fail (@str "Different error message:\n\
Expected: █{assertion->error_expected}█\n\
Got     : █{assertion->error_result  }█"))
          )
        ( (nequal (car assertion->body_result) assertion->body_expected)
          (fail (@str "Different output:\n\
Expected: █{assertion->body_expected}█\n\
Got     : █{(car assertion->body_result)}█"))
          )
        );cond
      );nif

    ;; Compare info messages with expectations
    (cond
      ;; No message expected
      ( (not assertion->info_expected)
        (unless (equal "" assertion->info_result)
          (fail (@str "Unexpected info message:\n\
Got     :█{assertion->info_result}█"))
          )
        )
      ( (not (match_messages assertion->info_expected assertion->info_result))
        (fail (@str "Different info message:\n\
Expected: █{assertion->info_expected}█\n\
Got     : █{assertion->info_result}█"))
        )
      );cond

    ;; Compare warn messages with expectations
    (cond
      ;; No message expected
      ( (not assertion->warn_expected)
        (unless (equal "" assertion->warn_result)
          (fail (@str "Unexpected warn message:\n\
Got     :█{assertion->warn_result}█"))
          )
        )
      ( (not (match_messages assertion->warn_expected assertion->warn_result))
        (fail (@str "Different warn message:\n\
Expected: █{assertion->warn_expected}█\n\
Got     : █{assertion->warn_result}█"))
        )
      );cond

    (@caseq assertion->status
      ;; Assertion failed, increase count and return nil
      ( fail
        nil
        )
      ;; Assertion passed, return t
      ( new
        (@set_status assertion 'pass)
        t )
      );caseq
    ;; Return assertion
    assertion
  );method

(defmethod @update_status ( ( test @test ) @rest _ )
  "Update TEST status according to its assertions results."
  (cond
    ( (not test->assertions)
      (@set_status test 'fail)
      (pushf "Test has no assertion." test->messages)
      )
    ( (forall assertion (@get_assertions test) (memq assertion->status '( pass skip )))
      (@set_status test 'pass)
      )
    ( t
      (@set_status test 'fail)
      )
    );cond
  ;; Return test
  test
  );method


;; =======================================================
;; Load/Run all tests and print report
;; =======================================================

(@fun @test_run_all
  ( @key
    ( test_files        ?type ( string ... )                 ?doc "Files containing tests (and/or definitions) those files are loaded."   )
    ( source_files      ?type ( string ... ) ?def test_files ?doc "Source files are used to make sure all global definitions are tested." )
    ( load_source_files ?type t|nil          ?def nil        ?doc "If non-nil, source files are also loaded."                             )
    @rest _
    )
  ?doc "Load all SKILL or SKILL++ FILES.
Run the defined tests.
Return t when at least one test is defined, all tests and all assertions passed.
Return nil otherwise."
  ?out t|nil
  (@letf ( ( (getShellEnvVar "SKILL_SHARP_RUN_TEST") "TRUE" )
           )
    ;; Reset existing tests.
    ;(_\@tests_reset)
    ;; Load source files when required
    (when load_source_files (mapcar '@load source_files))
    ;; Load all files containing tests
    (mapcar '@load test_files)
    ;; Run tests and return t or nil accordingly
    (@test_print_report ?globals (and source_files (car (@globals ?files source_files))))
    ));letf ;fun

