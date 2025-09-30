#!/bin/bash
#shellcheck disable=SC2016 #backquotes are printed on purpose

Describe 'sharp'
Describe 'lint'

It 'reports issues in `if`, `when`, `unless`'
When run ./bin/sharp lint ./metatest/lint/if_when_unless.scm
The stderr should include 'ERROR EXTRA_ARGS at line 9'
The stdout should include 'INFO IF_NIL at line 16'
The stderr should include 'WARNING STATIC_CONDITION at line 22  - `if` is useless, condition is static: t - (if t 12 27)'
The stderr should include 'WARNING STATIC_CONDITION at line 27  - `if` is useless, condition is static: nil - (if nil 12 27)'
The stderr should include 'WARNING STATIC_CONDITION at line 32  - `if` is useless, condition is static: 12 - (if 12 27 42)'
The stderr should include 'WARNING STATIC_CONDITION at line 37  - `when` is useless, condition is static:  - (when "" 12 27 42)'
The stderr should include 'WARNING STATIC_CONDITION at line 47  - `unless` is useless, condition is static:'" '( 12 ) - (unless '( 12 ) '( a b c ))"
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
The stderr should include 'WARNING STATIC_CONDITION at line 14'
The stdout should include 'INFO CAR_SETOF at line 15'
The stdout should include 'INFO CAR_SETOF at line 16'
The stdout should include 'INFO CAR_SETOF at line 25'
The stdout should include 'INFO CAR_SETOF at line 30'
The stdout should include 'INFO CAR_SETOF at line 34'
The stdout should include 'INFO CAR_SETOF at line 38'
The stdout should include 'INFO CAR_SETOF at line 41'
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

It 'stops at macros when reporting line numbers'
When run ./bin/sharp lint ./metatest/lint/line_numbers_in_macros.scm
The stdout should include 'INFO CAR_SETOF at line 16'
The stdout should include 'INFO CAR_SETOF at line 20'
The stdout should include 'INFO CAR_SETOF at line 25'
The stderr should be blank
The status should be success
End

It 'can be waived using `@no_lint`'
When run ./bin/sharp lint ./metatest/lint/waive.scm
The stdout should include 'INFO CAR_SETOF at line 5'
The stdout should not include 'INFO CAR_SETOF at line 8'
The stdout should not include 'INFO CAR_SETOF at line 9'
The stdout should include 'INFO IF_NIL at line 16'
The stderr should include 'WARNING STATIC_CONDITION at line 16'
The stdout should not include 'INFO IF_NIL at line 25'
The stderr should not include 'WARNING STATIC_CONDITION at line 25'
The stdout should not include 'INFO IF_NIL at line 36'
The stderr should not include 'WARNING STATIC_CONDITION at line 36'
The stderr should include 'ERROR EXTRA_ARGS at line 45'
The stderr should not include 'ERROR EXTRA_ARGS at line 53'
The stderr should not include 'ERROR EXTRA_ARGS at line 62'
The stdout should not include 'waived'
The stderr should not include 'waived'
The status should be failure
End

It 'reports extra positional arguments'
When run ./bin/sharp lint ./metatest/lint/extra_positional_arguments.scm
The stderr should include 'ERROR EXTRA_ARGS at line 5   - `getShellEnvVar`'
The stderr should include 'ERROR EXTRA_ARGS at line 7   - `if`'
The stderr should include 'ERROR EXTRA_ARGS at line 13  - `quote`'
The stdout should end with 'FAIL'
The status should be failure
End

It 'reports missing positional arguments'
When run ./bin/sharp lint ./metatest/lint/missing_positional_arguments.scm
The stderr should include 'ERROR MISSING_ARG at line 6   - `if`'
The stderr should include 'ERROR MISSING_ARG at line 6   - `getShellEnvVar`'
The stderr should include 'ERROR MISSING_ARG at line 8   - `quote`'
The stderr should include 'ERROR MISSING_ARG at line 12  - `prog1`'
The stderr should include 'ERROR MISSING_ARG at line 16  - `progn` requires 1 more positional arguments - (progn)'
The stdout should end with 'FAIL'
The status should be failure
End

It 'reports extra key arguments'
When run ./bin/sharp lint ./metatest/lint/extra_key_arguments.scm
The stderr should include 'WARNING EXTRA_KEY_ARG at line 2   - `let` extra key argument ?unexpected is provided'
The stderr should include 'WARNING POSITIONAL_KEY_ARG at line 4   - `progn` argument ?weird is treated as positional'
The stderr should include 'WARNING EXTRA_KEY_ARG at line 4   - `progn` extra key argument ?what'
The stderr should include 'WARNING EXTRA_KEY_ARG at line 7   - `@if` extra key argument ?extra_var'
The stderr should not include 'argument ?do_not_report'
The stdout should end with 'FAIL'
The status should be failure
End

It 'reports extra key arguments in local functions (as `@arglist` has to be deduced from function syntax)'
End

It 'reports missing required key arguments (SKILL# only, not a priority as they raise an error anyway)'
End

It 'reports syntax errors inside `let` definitions'
When run ./bin/sharp lint ./metatest/lint/let_syntax_errors.ils
The stderr should include 'ERROR SYNTAX_LET_BINDING at line 5   - `let` binding must be a symbol or symbol-value pair: (b) - (let ((a 12) (b) (c "str0" "str1") 42) (list a b 42))'
The stderr should include 'ERROR SYNTAX_LET_BINDING at line 6   - `let` binding must be a symbol or symbol-value pair: (c "str0" "str1") - (let ((a 12) (b) (c "str0" "str1") 42) (list a b 42))'
The stderr should include 'ERROR SYNTAX_LET_BINDING at line 7   - `let` binding must be a symbol or symbol-value pair: 42 - (let ((a 12) (b) (c "str0" "str1") 42) (list a b 42))'
The stderr should include 'ERROR MISSING_ARG at line 13  - `let` requires 1 more positional arguments - (let 12)'
The stderr should include 'ERROR SYNTAX_LET at line 13  - `let` first argument should be a list: 12 - (let 12)'
The stdout should end with 'FAIL'
The status should be failure
End

It 'reports messages inside `let` definitions'
When run ./bin/sharp lint ./metatest/lint/lint_errors_inside_let_definitions.il
The stdout should include 'INFO CAR_SETOF at line 4'
The stderr should include 'ERROR EXTRA_ARGS at line 10  - `setq`'
The stderr should include 'ERROR EXTRA_ARGS at line 20  - `setq`'
The stderr should include 'ERROR GLOBAL_USE at line 22  - Undefined global variable is used: abc'
The stdout should include 'INFO CAR_SETOF at line 24'
The status should be failure
End

It 'reports `letseq` that can be replaced by `let`'
End

It 'reports usage of global variables'
End

It 'reports unused variables'
When run ./bin/sharp lint ./metatest/lint/unused_variables.scm
The stderr should include 'WARNING LET_UNUSED at line 4   - `let` variable c is unused'
The stderr should include 'WARNING LET_ASSIGNED_ONLY at line 4   - `let` variable a is assigned only'
The stderr should include 'WARNING LET_UNUSED at line 12  - `let` variable unused_var is unused'
The stderr should include 'WARNING LET_UNUSED at line 20  - `let` variable unused_var is unused'
The stderr should include 'WARNING LET_UNUSED at line 20  - `let` variable another_unused_var is unused'
The stderr should include 'WARNING LET_UNUSED at line 29  - `let` variable twelve is unused'
The stderr should include 'WARNING LET_ASSIGNED_ONLY at line 35  - `let` variable assigned_with_set is assigned only'
The stdout should include 'INFO CAR_SETOF at line 42'
The status should be failure
End

It 'reports superseded variables'
When run ./bin/sharp lint ./metatest/lint/superseded_variables.ils
The stderr should include 'WARNING DEFUN_UNUSED at line 4   - `defun` variable a is unused'
The stderr should include 'WARNING LET_SUPERSEDE at line 6   - `let` variable a is superseded'
The stderr should include 'WARNING DEFUN_UNREACHABLE_VAR at line 20  - `defun` another argument is already called a'
The stdout should end with 'FAIL'
The status should be failure
End

It 'only reports wrong `status` and `sstatus` calls'
When run ./bin/sharp lint ./metatest/lint/sstatus.scm
The stderr should not include 'profCount'
The stderr should not include 'verboseLoad'
The stderr should not include 'verboseNamespace'
The stderr should include 'unknown_status_var'
The stderr should include 'thisDoesNotExist'
The stdout should end with 'FAIL'
The status should be failure
End

It 'reports `setof` where variable is never used'
End

It 'reports `exists` where variable is never used'
End

It 'reports static conditions in cond (except t)'
## This can cause confusion, example: (cond ( isCallable <name> ))
## Report all atoms except symbols.
## Report symbols (unless they are t or defined in a non-global environment)
End

It 'reports missing docstrings in functions'
When run ./bin/sharp lint ./metatest/lint/functions_without_docstrings.scm
The stderr should not include 'defun_with_docstring has no docstring'
The stderr should not include 'defglobalfun_with_docstring has no docstring'
The stderr should include '`defun` defun_without_docstring has no docstring'
The stderr should include '`defglobalfun` defglobalfun_without_docstring has no docstring'
The stdout should end with 'FAIL'
The status should be failure
End

It 'reports missing docstrings in procedures'
When run ./bin/sharp lint ./metatest/lint/procedures_without_docstrings.il
The stderr should not include 'procedure_with_docstring has no docstring'
The stderr should not include 'globalProc_with_docstring has no docstring'
The stderr should include '`procedure` procedure_without_docstring has no docstring'
The stderr should include '`globalProc` globalProc_without_docstring has no docstring'
The stdout should end with 'FAIL'
The status should be failure
End

It 'reports missing docstrings in `define`'
End

It 'reports missing docstrings in methods'
Skip 'methods are not supported yet'
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

It 'reports proper messages for `@str` calls'
When run ./bin/sharp lint ./metatest/lint/f-strings.scm
The stderr should include '*Error* @str: argument #1 should be a string'
The stderr should include "*Error* @str: too many arguments (1 expected, 2 given)"
The stderr should include '*Error* Open-bracket is never closed in f-string : \"Bracket is never closed { 12 27\"'
## TODO - Test more advanced f-string cases (never opened closing-bracket, brackets inside evaluated part, ...)
The stderr should include 'ERROR MACRO_EXPANSION at line 9   - `@str` error when expanding macro'
The stderr should include 'ERROR EXTRA_ARGS at line 13  - `@str` extra arguments are provided'
The stderr should include 'ERROR MACRO_EXPANSION at line 13  - `@str` error when expanding macro'
The stderr should include 'ERROR MACRO_EXPANSION at line 16  - `@str` error when expanding macro'
The stderr should not include '(@str "{var} {var}")'
The stdout should end with 'FAIL'
The status should be failure
End

It 'does not report variables in `@str` calls as unused'
When run ./bin/sharp lint ./metatest/lint/f-strings_variables.scm
The stderr should not include 'used_variable is unused'
The stderr should not include 'used_variable is assigned only'
The stderr should include 'WARNING LET_UNUSED at line 6   - `let` variable unused_var is unused'
The stdout should end with 'FAIL'
The status should be failure
End

It 'reports unused variables in anaphoric macros'
When run ./bin/sharp lint ./metatest/lint/anaphoric_macros.scm
The stderr should include 'WARNING @IF_UNUSED at line 7   - `@if` variable var_if is unused'
The stderr should include 'WARNING @NIF_UNUSED at line 11  - `@nif` variable var_nif is unused'
The stderr should include 'WARNING @WHEN_UNUSED at line 15  - `@when` variable var_when is unused'
The stdout should end with 'FAIL'
The status should be failure
End

It 'only reports wrong arguments in `lambda` and `defun` calls'
When run ./bin/sharp lint ./metatest/lint/symbol_as_fun_args.scm
The stderr should include 'ERROR SYNTAX_DEFUN at line 4'
The stdout should include 'INFO GLOBAL at line 9   - `defglobalfun` global function definition: valid_fun'
The stderr should include 'WARNING LAMBDA_UNUSED at line 13  - `lambda` variable args is unused'
The stderr should not include 'ERROR SYNTAX_DEFUN at line 9'
The stderr should not include 'variable _ is unused'
The stdout should end with 'FAIL'
The status should be failure
End

It 'reports unused variables in @with calls'
When run ./bin/sharp lint ./metatest/lint/with_variables.scm
The stderr should not include 'used_port is'
The stderr should include 'variable unused_var is unused'
The stdout should include 'INFO CAR_SETOF'
The stderr should include 'ERROR MACRO_EXPANSION'
The stderr should include '@with - DEFS cannot be nil'
The stdout should end with 'FAIL'
The status should be failure
End

It 'reports Lint messages inside `@letf`'
When run ./bin/sharp lint ./metatest/lint/letf_calls.scm
The stdout should include 'INFO CAR_SETOF at line 8'
The stderr should include 'ERROR MACRO_EXPANSION at line 23'
The stderr should include '@letf - DEFS cannot be nil'
The stdout should end with 'FAIL'
The status should be failure
End

It 'reports Lint messages inside `@wrap`'
When run ./bin/sharp lint ./metatest/lint/wrap_calls.scm
The stderr should include 'WARNING STATUS_FLAG at line 8   - `sstatus` unknown flag: ThisStatusDoesNotExist - (sstatus ThisStatusDoesNotExist t)'
The stderr should include 'WARNING STATUS_FLAG at line 9   - `sstatus` unknown flag: ThisStatusDoesNotExist - (sstatus ThisStatusDoesNotExist nil)'
The stderr should include 'WARNING STATUS_FLAG at line 10  - `sstatus` unknown flag: ANOTHER_ERRORFUL_STATUS - (sstatus ANOTHER_ERRORFUL_STATUS nil)'
The stdout should include 'INFO CAR_SETOF at line 12'
The stderr should include 'WARNING STATUS_FLAG at line 13  - `status` unknown flag: ANOTHER_ERRORFUL_STATUS - (status ANOTHER_ERRORFUL_STATUS)'
The stdout should include 'INFO EXTRA_WRAP at line 21  - `@wrap` without IN or OUT can be removed or replaced by `progn`'
The stdout should end with 'FAIL'
The status should be failure
End

It 'reports unused local functions'
When run ./bin/sharp lint ./metatest/lint/functions.scm
The stderr should include 'unused_defun_function0 is unused'
The stderr should include 'superseded_defun_function0 is unused'
The stderr should include 'superseded_defun_function1 is unused'
The stdout should include 'INFO GLOBAL at line 17  - `defglobalfun` global function definition: defglobalfun_function0'
The stdout should end with 'FAIL'
The status should be failure
End

It 'reports duplicated definitions'
When run ./bin/sharp lint ./metatest/lint/functions.scm
The stderr should include 'WARNING DEFUN_SUPERSEDE at line 13  - `defun` variable superseded_defun_function0 is superseded'
The stderr should include 'WARNING @FUN_SUPERSEDE at line 26  - `@fun` variable superseded_defun_function1 is superseded'
The stdout should end with 'FAIL'
The status should be failure
End


It 'reports usage of debugging functions'
When run ./bin/sharp lint ./metatest/lint/debugging_functions.scm
The stderr should include 'WARNING DEBUGGING at line 4   - `@show` debugging function should not be used in production - (\@show 12 27)'
The stderr should include 'WARNING DEBUGGING at line 9   - `pp` debugging function should not be used in production - (pp \@show)'
The stdout should end with 'FAIL'
The status should be failure
End

It 'supports `foreach` with mapping function'
End

It 'supports `foreach` with symbol and list as first argument'
End

It 'reports functions defined inside `prog` as global'
End

It 'do not report valid advanced usage of variables (local functions used before their definition, ...)'
End

It 'checks itself'
When run env SKILL_SHARP_LINT_HIDE_SEXPS=TRUE ./bin/sharp lint ./skill/autoloaded/lint.scm
The stdout should include 'INFO GLOBAL'
The stdout should end with 'PASS'
The stderr should be blank
The status should be success
End

End

End

