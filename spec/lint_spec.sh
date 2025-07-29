#!/bin/bash
Describe 'sharp'
Describe 'lint'

  It 'checks itself'
    When run ./bin/sharp lint ./skill/autoloaded/lint.scm
    The stdout should include 'INFO (IQ): IQ score is 100 (best is 100).'
    The stdout should end with 'with status PASS.'
    The stderr should be blank
    The status should be success
  End

  It 'can be waived using `@no_lint'"'"
    When run ./bin/sharp lint ./metatest/lint/waive.scm
    The stdout should include 'INFO (VAR16): ./metatest/lint/waive.scm, line 6 : declaration of variable reported_var supersedes previous declaration at line 5.'
    The stdout should not include 'ignored_var'
    The stdout should include "HINT (IF6): ./metatest/lint/waive.scm, line 19 : Remove the 'then nil' part and convert to an 'unless': (if t nil (quote then_part))"
    The stdout should not include 'waived_then_part'
    The stderr should be blank
    The status should be success
  End

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

  It 'reports calls to (car (setof ...))'
  When run ./bin/sharp lint ./metatest/lint/car_setof.scm
    The stdout should include 'HINT (CAR_SETOF): ./metatest/lint/car_setof.scm, line 6 (call_with) : (car (setof ...)) should be replaced by (car (exists ...)): (setof elt (quote (1 2 3)) (evenp elt))'
    The stderr should be blank
    The status should be success
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

