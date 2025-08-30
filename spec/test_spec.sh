#!/bin/bash
Describe 'sharp'
Describe 'test'


It 'asserts `@test'"'"' S-expressions only contain `@assertion'"'"' calls'
End


It 'raises an error when test has no function'
When run ./bin/sharp test ./metatest/test/without_function_test.scm
The stdout should be blank
The stderr should include "@test - ?fun is required, it should be a quoted symbol. (It can be set to 'nofun)"
The status should be failure
End


It 'raises an error when test has no title'
When run ./bin/sharp test ./metatest/test/without_title_test.scm
The stdout should be blank
The stderr should include "@test - ?title is required and should be a non-blank string when ?fun is 'nofun."
The status should be failure
End


It 'raises an error when test has missing documentation'
When run ./bin/sharp test ./metatest/test/missing_doc_test.scm
The stdout should be blank
The stderr should include '*Error* @test("Missing assertion documentation")'
The stderr should include 'either test or all assertions should be documented using ?doc argument.'
The status should be failure
End


It 'raises an error when assertion contains both ?out and ?error'
When run ./bin/sharp test ./metatest/test/wrong_test.scm
The stdout should be blank
The stderr should include '@assertion - Exactly one of ?out or ?error should be provided.'
The status should be failure
End


It 'works with simple test'
When run ./bin/sharp test ./metatest/test/simple_test.scm
The stdout should equal 'Total tests: 1
 - skipped tests: 0
 - passed  tests: 1
 - failed  tests: 0

Total assertions: 2
 - skipped assertions: 0
 - passed  assertions: 2
 - failed  assertions: 0

PASS'
The stderr should be blank
The status should be success
End


It 'reports extra info messages'
When run ./bin/sharp test ./metatest/test/extra_info_test.scm
The stdout should equal 'Total tests: 1
 - skipped tests: 0
 - passed  tests: 0
 - failed  tests: 1

Total assertions: 3
 - skipped assertions: 0
 - passed  assertions: 2
 - failed  assertions: 1

FAIL'
The stderr should include 'Unexpected info message:
Got     :█This is an unexpected info message.
█'
The status should be failure
End


It 'reports missing info messages'
When run ./bin/sharp test ./metatest/test/missing_info_test.scm
The stdout should equal 'Total tests: 1
 - skipped tests: 0
 - passed  tests: 0
 - failed  tests: 1

Total assertions: 3
 - skipped assertions: 0
 - passed  assertions: 2
 - failed  assertions: 1

FAIL'
The stderr should include 'Different info message:
Expected: █This is a missing info message.█
Got     : ██'
The status should be failure
End


It 'reports different info messages'
When run ./bin/sharp test ./metatest/test/different_info_test.scm
The stdout should equal 'Total tests: 1
 - skipped tests: 0
 - passed  tests: 0
 - failed  tests: 1

Total assertions: 3
 - skipped assertions: 0
 - passed  assertions: 2
 - failed  assertions: 1

FAIL'
The stderr should include 'Different info message:
Expected: █This is a different info message.█
Got     : █Different message than expected.█'
The status should be failure
End


It 'reports extra warn messages'
When run ./bin/sharp test ./metatest/test/extra_warn_test.scm
The stdout should equal 'Total tests: 1
 - skipped tests: 0
 - passed  tests: 0
 - failed  tests: 1

Total assertions: 3
 - skipped assertions: 0
 - passed  assertions: 2
 - failed  assertions: 1

FAIL'
The stderr should include 'Unexpected warn message:
Got     :█*WARNING* This is an unexpected warn message.
█'
The status should be failure
End


It 'reports missing warn messages'
When run ./bin/sharp test ./metatest/test/missing_warn_test.scm
The stdout should equal 'Total tests: 1
 - skipped tests: 0
 - passed  tests: 0
 - failed  tests: 1

Total assertions: 3
 - skipped assertions: 0
 - passed  assertions: 2
 - failed  assertions: 1

FAIL'
The stderr should include 'Different warn message:
Expected: █This is a missing warn message.█
Got     : ██'
The status should be failure
End


It 'reports different warn messages'
When run ./bin/sharp test ./metatest/test/different_warn_test.scm
The stdout should equal 'Total tests: 1
 - skipped tests: 0
 - passed  tests: 0
 - failed  tests: 1

Total assertions: 3
 - skipped assertions: 0
 - passed  assertions: 2
 - failed  assertions: 1

FAIL'
The stderr should include 'Different warn message:
Expected: █This is a different warn message.█
Got     : █*WARNING* Different message than expected.█'
The status should be failure
End


It 'reports extra error messages'
When run ./bin/sharp test ./metatest/test/extra_error_test.scm
The stdout should equal 'Total tests: 1
 - skipped tests: 0
 - passed  tests: 0
 - failed  tests: 1

Total assertions: 3
 - skipped assertions: 0
 - passed  assertions: 2
 - failed  assertions: 1

FAIL'
The stderr should include 'Unexpected error:
Got     :█*Error* This is an unexpected error message.

█'
The status should be failure
End


It 'reports missing error messages'
When run ./bin/sharp test ./metatest/test/missing_error_test.scm
The stdout should equal 'Total tests: 1
 - skipped tests: 0
 - passed  tests: 0
 - failed  tests: 1

Total assertions: 3
 - skipped assertions: 0
 - passed  assertions: 2
 - failed  assertions: 1

FAIL'
The stderr should include 'Different error message:
Expected: █This is a missing error message.█
Got     : ██'
The status should be failure
End


It 'reports different error messages'
When run ./bin/sharp test ./metatest/test/different_error_test.scm
The stdout should equal 'Total tests: 1
 - skipped tests: 0
 - passed  tests: 0
 - failed  tests: 1

Total assertions: 3
 - skipped assertions: 0
 - passed  assertions: 2
 - failed  assertions: 1

FAIL'
The stderr should include 'Different error message:
Expected: █This is a different error message.█
Got     : █*Error* Different message than expected.
█'
The status should be failure
End


It 'supports test environments'
When run ./bin/sharp test ./metatest/test/environments_test.scm
The stdout should equal 'Total tests: 2
 - skipped tests: 0
 - passed  tests: 1
 - failed  tests: 1

Total assertions: 4
 - skipped assertions: 0
 - passed  assertions: 3
 - failed  assertions: 1

FAIL'
The stderr should include 'Unexpected error:
Got     :█*Error* eval: undefined function - no_args
█'
The status should be failure
End


It 'works with example tests'
When run ./bin/sharp test ./metatest/test/examples_test.scm
The stdout should equal 'Running assertions - BEGIN
Running assertions - END
Total tests: 4
 - skipped tests: 1
 - passed  tests: 2
 - failed  tests: 1

Total assertions: 10
 - skipped assertions: 2
 - passed  assertions: 7
 - failed  assertions: 1

FAIL'
The stderr should include '
Unexpected warn message:
Got     :█*WARNING* warn message
█

Unexpected info message:
Got     :█info message
█

Unexpected error:
Got     :█*Error* error message

█'
The status should be failure
End


It 'raises errors when wrong key arguments are used in `@test'"'"
When run ./bin/sharp test ./metatest/test/invalid_test_argument_test.scm
The stdout should be blank
The stderr should include '*Error* @test - Unrecognized key argument: ?skipped'
The status should be failure
End


It 'raises errors when wrong key arguments are used in `@assertion'"'"
When run ./bin/sharp test ./metatest/test/invalid_assertion_argument_test.scm
The stdout should equal 'Total tests: 1
 - skipped tests: 0
 - passed  tests: 0
 - failed  tests: 1

Total assertions: 1
 - skipped assertions: 0
 - passed  assertions: 0
 - failed  assertions: 0

FAIL'
The stderr should include 'Error occured when running test: ("*Error* @assertion - Unrecognized key argument: ?skipped")'
The status should be failure
End


It 'reports functions which are not used nor referenced inside test or assertions body'
End


It 'raise an error when defining two tests for the same function'
When run ./bin/sharp test ./metatest/test/already_tested_fun.scm
The stdout should be blank
The stderr should not include 'local_fun'
The stderr should include '*Error* Function global_fun is already tested by @test'
The status should be failure
End


End
End

