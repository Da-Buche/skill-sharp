#!/bin/bash
Describe 'sharp'
Describe 'globals'

It 'is executable'
When run test -x ./bin/globals
The stdout should be blank
The stderr should be blank
The status should be success
End

It 'reports global SKILL variables'
When run ./bin/sharp globals ./metatest/globals/variables.il
The stdout should equal $'nil\n(global_variable nonlocal)\nnil\nnil\n(global_symbol)'
The stderr should be blank
The status should be success
End

It 'reports global Scheme variables'
When run ./bin/sharp globals ./metatest/globals/variables.ils
The stdout should equal $'nil\nnil\n(global_variable nonlocal)\nnil\n(global_symbol)'
The stderr should be blank
The status should be success
End

It 'reports global SKILL functions'
When run ./bin/sharp globals ./metatest/globals/functions.il
The stdout should equal $'(local_fun nonlocal global_fun)\n(lambda_variable)\nnil\nnil\nnil'
The stderr should be blank
The status should be success
End

It 'reports global Scheme functions'
When run ./bin/sharp globals ./metatest/globals/functions.ils
The stdout should equal $'(lambda_variable nonlocal global_fun)\nnil\nnil\nnil\nnil'
The stderr should be blank
The status should be success
End

It 'reports global SKILL classes and methods'
When run ./bin/sharp globals ./metatest/globals/classes.il
The stdout should equal $'(global_method local_method)\nnil\nnil\n(global_class local_class)\n(global_class local_class)'
The stderr should be blank
The status should be success
End

It 'reports global Scheme classes and methods'
When run ./bin/sharp globals ./metatest/globals/classes.ils
The stdout should equal $'(global_method local_method)\nnil\nnil\n(global_class local_class)\n(global_class local_class)'
The stderr should be blank
The status should be success
End

It 'reports global definitions'
When run ./bin/sharp globals ./metatest/globals/definitions.scm
The stdout should equal '(global_skill_function skill_method local_skill_function global_scheme_function nonlocal_scheme_function scheme_method)
(only_skill_var global_skill_var imported_skill_var)
(global_skill_var imported_skill_var only_scheme_var)
(skill_class scheme_class)
(skill_class skill_container scheme_class scheme_container)'
The stderr should be blank
The status should be success
End


End
End

