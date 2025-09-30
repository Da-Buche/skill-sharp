#!/bin/bash
Describe 'sharp'
Describe 'globals'

It 'is executable'
When run env SKILL_SHARP_GLOBALS_LOAD=TRUE test -x ./bin/globals
The stdout should be blank
The stderr should be blank
The status should be success
End

It 'reports global SKILL variables'
When run env SKILL_SHARP_GLOBALS_LOAD=TRUE SKILL_SHARP_GLOBALS_SHOW_PROPS=TRUE ./bin/sharp globals ./metatest/globals/variables.il
The stdout should equal $'nil\n(global_variable nonlocal)\nnil\nnil\n(global_symbol)'
The stderr should be blank
The status should be success
End

It 'reports global Scheme variables'
When run env SKILL_SHARP_GLOBALS_LOAD=TRUE SKILL_SHARP_GLOBALS_SHOW_PROPS=TRUE ./bin/sharp globals ./metatest/globals/variables.ils
The stdout should equal $'nil\nnil\n(global_variable nonlocal)\nnil\n(global_symbol)'
The stderr should be blank
The status should be success
End

It 'reports global SKILL functions'
When run env SKILL_SHARP_GLOBALS_LOAD=TRUE SKILL_SHARP_GLOBALS_SHOW_PROPS=TRUE ./bin/sharp globals ./metatest/globals/functions.il
The stdout should equal $'(global_fun local_fun nonlocal)\n(lambda_variable)\nnil\nnil\nnil'
The stderr should be blank
The status should be success
End

It 'reports global Scheme functions'
When run env SKILL_SHARP_GLOBALS_LOAD=TRUE ./bin/sharp globals ./metatest/globals/functions.ils
The stdout should equal $'(global_fun lambda_variable nonlocal)\nnil\nnil\nnil'
The stderr should be blank
The status should be success
End

It 'reports global SKILL classes and methods'
When run env SKILL_SHARP_GLOBALS_LOAD=TRUE ./bin/sharp globals ./metatest/globals/classes.il
The stdout should equal $'(global_method local_method)\nnil\nnil\n(global_class local_class)'
The stderr should be blank
The status should be success
End

It 'reports global Scheme classes and methods'
When run env SKILL_SHARP_GLOBALS_LOAD=TRUE SKILL_SHARP_GLOBALS_SHOW_PROPS=TRUE ./bin/sharp globals ./metatest/globals/classes.ils
The stdout should equal $'(global_method local_method)\nnil\nnil\n(global_class local_class)\n(global_class local_class)'
The stderr should be blank
The status should be success
End

It 'reports global definitions'
When run env SKILL_SHARP_GLOBALS_LOAD=TRUE SKILL_SHARP_GLOBALS_SHOW_PROPS=TRUE ./bin/sharp globals ./metatest/globals/definitions.scm
The stdout should equal '(global_scheme_function global_skill_function local_skill_function nonlocal_scheme_function scheme_method skill_method)
(global_skill_var imported_skill_var only_skill_var)
(global_skill_var imported_skill_var only_scheme_var)
(scheme_class skill_class)
(scheme_class scheme_container skill_class skill_container)'
The stderr should be blank
The status should be success
End

## Same tests but using Lint to detect global definitions

It 'reports global SKILL variables (no load)'
When run env SKILL_SHARP_GLOBALS_SHOW_PROPS=TRUE ./bin/sharp globals ./metatest/globals/variables.il
The stdout should equal $'nil\n(global_variable nonlocal)\nnil\nnil\n(global_symbol)'
The stderr should be blank
The status should be success
End

It 'reports global Scheme variables (no load)'
When run ./bin/sharp globals ./metatest/globals/variables.ils
The stdout should equal $'nil\nnil\n(global_variable nonlocal)\nnil'
The stderr should be blank
The status should be success
End

It 'reports global SKILL functions (no load)'
When run ./bin/sharp globals ./metatest/globals/functions.il
The stdout should equal $'(global_fun local_fun nonlocal)\n(lambda_variable)\nnil\nnil'
The stderr should be blank
The status should be success
End

It 'reports global Scheme functions (no load)'
When run env SKILL_SHARP_GLOBALS_SHOW_PROPS=TRUE ./bin/sharp globals ./metatest/globals/functions.ils
The stdout should equal $'(global_fun lambda_variable nonlocal)\nnil\nnil\nnil\nnil'
The stderr should be blank
The status should be success
End

It 'reports global SKILL classes and methods (no load)'
Pending
When run env SKILL_SHARP_GLOBALS_SHOW_PROPS=TRUE ./bin/sharp globals ./metatest/globals/classes.il
The stdout should equal $'(global_method local_method)\nnil\nnil\n(global_class local_class)\n(global_class local_class)'
The stderr should be blank
The status should be success
End

It 'reports global Scheme classes and methods (no load)'
Pending
When run ./bin/sharp globals ./metatest/globals/classes.ils
The stdout should equal $'(global_method local_method)\nnil\nnil\n(global_class local_class)'
The stderr should be blank
The status should be success
End

It 'reports global definitions (no load)'
Pending
When run env SKILL_SHARP_GLOBALS_SHOW_PROPS=TRUE ./bin/sharp globals ./metatest/globals/definitions.scm
The stdout should equal '(global_scheme_function global_skill_function local_skill_function nonlocal_scheme_function scheme_method skill_method)
(global_skill_var imported_skill_var only_skill_var)
(global_skill_var imported_skill_var only_scheme_var)
(scheme_class skill_class)
(scheme_class scheme_container skill_class skill_container)'
The stderr should be blank
The status should be success
End

## Check all SKILL# autoloaded files

Parameters:dynamic
  # shellcheck disable=SC2044 # shellspec %data directive cannot be used inside -exec
  for file in $(find skill/autoloaded -name '*.il' -o -name '*.ils' -o -name '*.scm') ; do
    %data "$file"
  done
End

It "reports same global definitions with Lint and Load in $1"
When run bash -c "diff <(./bin/sharp globals $1)\
 <(env SKILL_SHARP_GLOBALS_LOAD=TRUE                              \
       SKILL_SHARP_BEFORE_COMMAND='(load \"./skill/loader.scm\")' \
       ./bin/sharp globals $1)"
The stdout should be blank
The stderr should be blank
The status should be success
End


End
End

