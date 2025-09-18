#!/bin/bash
Describe 'sharp'

Describe 'lint'

It 'reports issues in `if`, `when`, `unless`'
When run ./bin/sharp lint ./metatest/lint/if_when_unless.scm
The stdout should include 'ERROR EXTRA_ARGS at line 9'
The stdout should include 'INFO IF_NIL at line 16'
The stdout should include 'WARNING STATIC_CONDITION at line 22  - `if` is useless, condition is static: t - (if t 12 27)'
The stdout should include 'WARNING STATIC_CONDITION at line 27  - `if` is useless, condition is static: nil - (if nil 12 27)'
The stdout should include 'WARNING STATIC_CONDITION at line 32  - `if` is useless, condition is static: 12 - (if 12 27 42)'
The stdout should include 'WARNING STATIC_CONDITION at line 37  - `when` is useless, condition is static:  - (when "" 12 27 42)'
The stdout should include 'WARNING STATIC_CONDITION at line 47  - `unless` is useless, condition is static:'" '( 12 ) - (unless '( 12 ) '( a b c ))"
The stderr should be blank
The status should be failure
End

It 'reports calls to (car (setof ...))'
When run ./bin/sharp lint ./metatest/lint/car_setof.scm
The stdout should include 'INFO CAR_SETOF at line 5   - (car (setof ...)) can almost always be replaced by (car (exists ...)) - (setof elt (list 1 2 3) (evenp elt))'
The stderr should be blank
The status should be success
End

It 'reports line numbers'
When run ./bin/sharp lint ./metatest/lint/line_numbers.scm
The stdout should include 'INFO CAR_SETOF at line 7'
The stdout should include 'WARNING STATIC_CONDITION at line 14'
The stdout should include 'INFO CAR_SETOF at line 15'
The stdout should include 'INFO CAR_SETOF at line 16'
The stdout should include 'INFO CAR_SETOF at line 25'
The stdout should include 'INFO CAR_SETOF at line 30'
The stdout should include 'INFO CAR_SETOF at line 34'
The stdout should include 'INFO CAR_SETOF at line 38'
The stdout should include 'INFO CAR_SETOF at line 41'
The stderr should be blank
The status should be failure
End

It 'reports line numbers with C-style syntax'
When run ./bin/sharp lint ./metatest/lint/line_numbers_c_style.scm
Skip 'I do not care about C-style for now...'
The stdout should include 'INFO CAR_SETOF at line 7'
The stdout should include 'INFO CAR_SETOF at line 15'
The stdout should include 'INFO CAR_SETOF at line 16'
The stdout should include 'INFO CAR_SETOF at line 25'
The stdout should include 'INFO CAR_SETOF at line 30'
The stdout should include 'INFO CAR_SETOF at line 34'
The stdout should include 'INFO CAR_SETOF at line 38'
The stdout should include 'INFO CAR_SETOF at line 41'
The stderr should be blank
The status should be success
End

It 'reports line numbers inside complicated macros'
End

It 'can be waived using `@no_lint'"'"
When run ./bin/sharp lint ./metatest/lint/waive.scm
The stdout should include 'INFO CAR_SETOF at line 5'
The stdout should not include 'INFO CAR_SETOF at line 8'
The stdout should not include 'INFO CAR_SETOF at line 9'
The stdout should include 'INFO IF_NIL at line 16'
The stdout should include 'WARNING STATIC_CONDITION at line 16'
The stdout should not include 'INFO IF_NIL at line 25'
The stdout should not include 'WARNING STATIC_CONDITION at line 25'
The stdout should not include 'INFO IF_NIL at line 36'
The stdout should not include 'WARNING STATIC_CONDITION at line 36'
The stdout should include 'ERROR EXTRA_ARGS at line 45'
The stdout should not include 'ERROR EXTRA_ARGS at line 53'
The stdout should not include 'ERROR EXTRA_ARGS at line 62'
The stdout should not include 'waived'
The stderr should be blank
The status should be failure
End

It 'reports extra positional arguments'
When run ./bin/sharp lint ./metatest/lint/extra_positional_arguments.scm
The stdout should include 'ERROR EXTRA_ARGS at line 5   - `if`'
The stdout should include 'ERROR EXTRA_ARGS at line 5   - `getShellEnvVar`'
The stdout should include 'ERROR EXTRA_ARGS at line 11  - `quote`'
The stderr should be blank
The status should be failure
End

It 'reports missing positional arguments'
When run ./bin/sharp lint ./metatest/lint/missing_positional_arguments.scm
The stdout should include 'ERROR MISSING_ARG at line 6   - `if`'
The stdout should include 'ERROR MISSING_ARG at line 6   - `getShellEnvVar`'
The stdout should include 'ERROR MISSING_ARG at line 8   - `quote`'
The stdout should include 'ERROR MISSING_ARG at line 12  - `prog1`'
The stdout should include 'ERROR MISSING_ARG at line 16  - `progn` requires 1 more positional arguments - (progn)'
The stderr should be blank
The status should be failure
End

It 'reports extra key arguments'
When run ./bin/sharp lint ./metatest/lint/extra_key_arguments.scm
The stdout should include 'WARNING EXTRA_KEY_ARG at line 2   - `let` extra key argument ?unexpected is provided'
The stdout should include 'WARNING POSITIONAL_KEY_ARG at line 4   - `progn` argument ?weird is treated as positional'
The stdout should include 'WARNING EXTRA_KEY_ARG at line 4   - `progn` extra key argument ?what'
The stdout should include 'WARNING EXTRA_KEY_ARG at line 7   - `\@if` extra key argument ?extra_var'
The stdout should not include 'argument ?do_not_report'
The stderr should be blank
The status should be failure
End

It 'reports extra key arguments in local functions'
End

It 'reports missing required key arguments (SKILL# only, not a priority as they raise an error anyway)'
End

It 'reports wrong `let` definitions'
When run ./bin/sharp lint ./metatest/lint/let_errors.ils
The stdout should include 'ERROR LET_DEF_SYNTAX at line 5   - `let` binding must be a symbol or symbol-value pair: (b) - (let ((a 12) (b) (c "str0" "str1") 42) (list a b 42))'
The stdout should include 'ERROR LET_DEF_SYNTAX at line 6   - `let` binding must be a symbol or symbol-value pair: (c "str0" "str1") - (let ((a 12) (b) (c "str0" "str1") 42) (list a b 42))'
The stdout should include 'ERROR LET_DEF_SYNTAX at line 7   - `let` binding must be a symbol or symbol-value pair: 42 - (let ((a 12) (b) (c "str0" "str1") 42) (list a b 42))'
The stdout should include 'ERROR MISSING_ARG at line 13  - `let` requires 1 more positional arguments - (let 12)'
The stdout should include 'ERROR LET_SYNTAX at line 13  - `let` first argument should be a list: 12 - (let 12)'
The stderr should be blank
The status should be failure
End

It 'reports errors inside `let` definitions'
End

It 'reports unused variables'
When run ./bin/sharp lint ./metatest/lint/unused_variables.scm
The stdout should include 'WARNING LET_UNUSED at line 4   - `let` variable c is unused'
# The stdout should include 'WARNING ASSIGNED_ONLY at line 4 - variable a is assigned but unused'
# The stdout should include 'WARNING UNUSED at line 12 - variable unused_var is unused'
# The stdout should include 'WARNING ASSIGNED_ONLY at line 20 - variable unused_var is assigned but unused'
# The stdout should include 'WARNING UNUSED at line 20 - variable another_unused_var is unused'
# The stdout should include 'WARNING ASSIGNED_ONLY at line 29 - variable twelve is assigned but unused'
The stderr should be blank
The status should be failure
End

It 'reports superseded variables'
End

It 'checks itself'
Skip 'for now, this will be the final test to implement all required checks'
When run ./bin/sharp lint ./skill/autoloaded/lint.scm
The stdout should end with 'PASS'
The stderr should be blank
The status should be success
End



End






Describe 'old_lint'

Skip "Lint is not working properly yet (it seems it's behavior is limited)"




  It 'only reports wrong `status'"'"' and `sstatus'"'"' calls'
    When run ./bin/sharp lint ./metatest/lint/sstatus.scm
    The stdout should not include 'profCount'
    The stdout should not include 'verboseLoad'
    The stdout should not include 'verboseNamespace'
    The stdout should include 'unknown_status_var'
    The stdout should include 'thisDoesNotExist'
    The stderr should be blank
    The status should be failure
  End

  It 'reports missing docstrings in methods'
    When run ./bin/sharp lint ./metatest/lint/methods_without_docstrings.scm
    The stdout should not include 'method with_docstring has no docstring'
    The stdout should not include 'method with_docstring_before has no docstring'
    The stdout should not include 'method with_docstring_after has no docstring'
    The stdout should not include 'method with_docstring_around has no docstring'
    The stdout should include 'method no_docstring has no docstring'
    The stdout should include 'method no_docstring_before has no docstring'
    The stdout should include 'method no_docstring_after has no docstring'
    The stdout should include 'method no_docstring_around has no docstring'
    The stderr should be blank
    The status should be failure
  End

  It 'reports missing docstrings in functions'
    When run ./bin/sharp lint ./metatest/lint/functions_without_docstrings.scm
    The stdout should not include 'defun_with_docstring'
    The stdout should not include 'defglobalfun_with_docstring'
    The stdout should include 'function defun_without_docstring has no docstring'
    The stdout should include 'function defglobalfun_without_docstring has no docstring'
    The stderr should be blank
    The status should be failure
  End

  It 'reports missing docstrings in procedures'
    When run ./bin/sharp lint ./metatest/lint/procedures_without_docstrings.il
    The stdout should not include 'procedure_with_docstring'
    The stdout should not include 'globalProc_with_docstring'
    The stdout should include 'procedure procedure_without_docstring has no docstring'
    The stdout should include 'procedure globalProc_without_docstring has no docstring'
    The stderr should be blank
    The status should be failure
  End

  It 'reports proper messages for `@str'"' calls"
    When run ./bin/sharp lint ./metatest/lint/f-strings.scm
    The stdout should include '*Error* @str: argument #1 should be a string (type template = "t") - str'
    The stdout should include '*Error* @str: too many arguments (1 expected, 2 given) - ("{12+27} %N" (quote test))'
    The stdout should include '*Error* Open-bracket is never closed in f-string : "Bracket is never closed { 12 27"'
    ## TODO - Test more advanced f-string cases (never opened closing-bracket, brackets inside evaluated part, ...)
    The stdout should not include '(@str {var} {var})'
    The stderr should be blank
    The status should be failure
  End

  It 'does not report variables in `@str'"'"' calls as unused'
    When run ./bin/sharp lint ./metatest/lint/f-strings_variables.scm
    The stdout should include 'UNUSED VAR (Unused): ./metatest/lint/f-strings_variables.scm, line 6 : variable unused_var does not appear to be referenced. (assigned only)'
    The stderr should be blank
    The status should be success
  End

  It 'reports unused variables in anaphoric macros'
    When run ./bin/sharp lint ./metatest/lint/anaphoric_macros.scm
    The stdout should include 'variable var_if does not appear to be referenced.'
    The stdout should include 'variable var_nif does not appear to be referenced.'
    The stdout should include 'variable var_when does not appear to be referenced.'
    The stderr should be blank
    The status should be success
  End

  It 'reports unused variables in @with calls'
    When run ./bin/sharp lint ./metatest/lint/with_variables.scm
    The stdout should not include 'used_port'
    The stdout should include 'UNUSED VAR (Unused): ./metatest/lint/with_variables.scm, line 6 (call_with) : variable unused_var does not appear to be referenced.'
    The stdout should include 'HINT (CAR_SETOF): ./metatest/lint/with_variables.scm, line 9 (call_with) : (car (setof ...)) should be replaced by (car (exists ...)): (setof elt (quote (1 2 3)) (evenp elt))'
    #The stdout should include 'HINT (EXTRA_WITH): ./metatest/lint/with_variables.scm, line 19 (call_without_definitions) : @with without definitions can be replaced by let or progn'
    The stdout should include '@with - DEFS cannot be nil'
    The stderr should be blank
    The status should be failure
  End

  It 'reports Lint messages inside `@letf'"'"
    When run ./bin/sharp lint ./metatest/lint/letf_calls.scm
    The stdout should include 'HINT (CAR_SETOF): ./metatest/lint/letf_calls.scm, line 11 (call_with_hint) : (car (setof ...)) should be replaced by (car (exists ...)): (setof elt (quote (1 2 3)) (evenp elt))'
    #The stdout should include 'HINT (EXTRA_LETF): ./metatest/lint/letf_calls.scm, line 23 (call_without_definitions) : @letf without definitions can be replaced by let or progn'
    The stdout should include '@letf - DEFS cannot be nil'
    The stderr should be blank
    The status should be failure
  End

  It 'reports Lint messages inside `@wrap'"'"
  When run ./bin/sharp lint ./metatest/lint/wrap_calls.scm
    The stdout should include 'ERROR (UNKNOWN_STATUS_FLAG): ./metatest/lint/wrap_calls.scm, line 8 (another_dummy_fun) : Unknown (s)status flag: (sstatus ThisStatusDoesNotExist t)'
    The stdout should include 'ERROR (UNKNOWN_STATUS_FLAG): ./metatest/lint/wrap_calls.scm, line 9 (another_dummy_fun) : Unknown (s)status flag: (sstatus ThisStatusDoesNotExist nil)'
    The stdout should include 'ERROR (UNKNOWN_STATUS_FLAG): ./metatest/lint/wrap_calls.scm, line 10 (another_dummy_fun) : Unknown (s)status flag: (sstatus ANOTHER_ERRORFUL_STATUS nil)'
    The stdout should include 'HINT (CAR_SETOF): ./metatest/lint/wrap_calls.scm, line 12 (another_dummy_fun) : (car (setof ...)) should be replaced by (car (exists ...)): (setof elt (quote (1 2 3)) (evenp elt))'
    The stdout should include 'ERROR (UNKNOWN_STATUS_FLAG): ./metatest/lint/wrap_calls.scm, line 13 (another_dummy_fun) : Unknown (s)status flag: (status ANOTHER_ERRORFUL_STATUS)'
    #The stdout should include 'HINT (EXTRA_WRAP): ./metatest/lint/wrap_calls.scm, line 9 (dummy_fun) : @wrap without IN or OUT can be removed or replaced by progn'
    The stderr should be blank
    The status should be failure
  End

  It 'reports extra use of `@wrap'"'"
  When run ./bin/sharp lint ./metatest/lint/empty_wrap.scm
    The stdout should include 'HINT (EXTRA_WRAP): ./metatest/lint/empty_wrap.scm, line 7 (dummy_fun) : @wrap without IN or OUT can be removed or replaced by progn'
    The stderr should be blank
    The status should be success
  End

  It 'only reports wrong arguments in `lambda'"'"' and `defun'"'"' calls'
  End

  It 'reports unused local functions'
  End

  It 'reports duplicated definitions'
  End

  It 'reports usage of debugging functions'
  When run ./bin/sharp lint ./metatest/lint/debugging_functions.scm
    The stdout should include '@show is a debugging function and should not be used in production'
    The stdout should include 'pp is a debugging function and should not be used in production'
    The stderr should be blank
    The status should be failure
  End


End

End

