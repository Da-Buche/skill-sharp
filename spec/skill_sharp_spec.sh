#!/bin/bash
Describe 'skill#'

## Requirements
missing_fd() { ! fd  --version >/dev/null 2>&1; }

It 'passes Lint (all files together)'
When run bash -c "SKILL_SHARP_LINT_HIDE_SEXPS=TRUE SKILL_SHARP_LINT_HIDE_IGNORES=NOT_CALLABLE ./bin/sharp lint ./skill"
The stdout should end with 'PASS'
The stderr should be blank
The status should be success
End


Describe 'lint'
Skip if "fd not available" missing_fd

  Parameters:dynamic
  for file in $(fd '(.scm|.ils?)$' skill) ; do
    %data "${file}"
  done
  End

  It "passes for $1"
  When run bash -c "SKILL_SHARP_LINT_HIDE_SEXPS=TRUE SKILL_SHARP_LINT_HIDE_IGNORES=NOT_CALLABLE ./bin/sharp lint $1"
  The stdout should end with 'PASS'
  The stderr should be blank
  The status should be success
  End

End

Describe 'unit-tests'

  It 'passes unit-tests using SKILL interpreter'
  When run ./bin/sharp test ./skill ./test
  The stdout should end with 'PASS'
  The stderr should be blank
  The status should be success
  End

  It 'passes unit-tests (including in-code ones) using SKILL interpreter'
  When run env SKILL_SHARP_RUN_TEST=TRUE ./bin/sharp test ./skill ./test
  The stdout should end with 'PASS'
  The stderr should be blank
  The status should be success
  End

  It 'passes unit-tests (including in-code ones) using SKILL interpreter with strict Type-Checking'
  When run env SKILL_SHARP_RUN_TEST=TRUE SKILL_SHARP_STRICT_TYPE_CHECKING=TRUE ./bin/sharp test ./skill ./test
  The stdout should end with 'PASS'
  The stderr should be blank
  The status should be success
  End

  It 'passes unit-tests using cdsmps'
  When run env SKILL_INTERPRETER="$CDS_INST_DIR/tools.lnx86/bin/cdsmps" \
               SKILL_SHARP_RUN_TEST=TRUE                                \
               ./bin/sharp test ./skill ./test
  The stdout should end with 'PASS'
  The stderr should be blank
  The status should be success
  End

  It 'passes unit-tests using SKILL interpreter (sstatus debugMode t)'
  End

  It 'passes unit-tests using SKILL interpreter (rexMagic nil)'
  End

  It 'passes unit-tests using Virtuoso -nograph'
  Skip if "virtuoso not executable" test ! -x "$CDS_INST_DIR/tools.lnx86/dfII/bin/64bit/virtuoso"
  End

End

It 'has correct syntax'
End


It 'does not contain common typos (TOOD, @left, ...)'
End


It 'fetches defined functions from files'
End


It 'deduces global function prefix from files'
End


It 'reports local functions with global prefix'
End


It 'reports global functions without prefix'
End


It 'cleans documentation'
When run bash -c 'mkdir -p "./doc/finder/SKILL/SKILL#/" && rm -f "./doc/finder/SKILL/SKILL#/sharp.fnd"'
The stdout should be blank
The stderr should be blank
The status should be success
The file ./doc/finder/SKILL/SKILL#/sharp.fnd should not be exist
End

It 'generates documentation'
When run env SKILL_SHARP_TRACK_SOURCE=TRUE \
           bash -c 'mkdir -p ./doc/finder/SKILL/SKILL#/ && ./bin/sharp docgen ./skill/loader.scm ./test > "./doc/finder/SKILL/SKILL#/sharp.fnd"'
The file './doc/finder/SKILL/SKILL#/sharp.fnd' should be exist
The stdout should be blank
The stderr should be blank
The status should be success
End

It 'generated valid documentation'
Skip if ".fnd not readable" test ! -r ./doc/finder/SKILL/SKILL#/sharp.fnd
When run ./bin/sharp fndcheck ./doc/finder/SKILL/SKILL#/sharp.fnd
The stdout should be blank
The stderr should be blank
The status should be success
End

It 'generated full documentation'
Skip if ".fnd not readable" test ! -r ./doc/finder/SKILL/SKILL#/sharp.fnd
When run grep -n 'Missing documentation for function' './doc/finder/SKILL/SKILL#/sharp.fnd'
The stdout should be blank
The stderr should be blank
The status should be failure
End



## =======================================================
## Executable and associated commands
## =======================================================

Describe 'sharp'

  It 'is executable'
    When run test -x ./bin/sharp
    The stdout should be blank
    The stderr should be blank
    The status should be success
  End

  It 'matches sharp.scm (symlink is not broken)'
    When run diff ./bin/sharp ./skill/sharp.scm
    The stdout should be blank
    The stderr should be blank
    The status should be success
  End

End


End

