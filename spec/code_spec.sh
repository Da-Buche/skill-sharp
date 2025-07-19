#!/bin/bash
Describe 'code'

## Requirements
missing_scc() { ! scc --version >/dev/null 2>&1; }
missing_fd()  { ! fd  --version >/dev/null 2>&1; }

list_files() {
  ## List all project source files
  fd . --hidden --no-require-git --type f --exclude '*.png' "$@"
}

export -f list_files


It 'contains no "TODO"'
Skip if "fd not available" missing_fd
Pending "TODOs are not failures but they should be reminded."
# shellcheck disable=SC2046 # Intended word splitting
When run grep -Eni 'TO-?DO' $(list_files --exclude='code_spec.sh')
The stdout should be blank
The stderr should be blank
The status should be failure
End


It 'contains no "DEBUG"'
Skip if "fd not available" missing_fd
# shellcheck disable=SC2016 # Intended quoted variable
When run env FILES="$(list_files --exclude='code_spec.sh' --exclude='Makefile' --exclude='README.md')" bash -c 'grep -Eni "\bDEBUG\b" $FILES | sed "s/@debug//g" | grep -Ei "\bDEBUG\b"'
The stdout should be blank
The stderr should be blank
The status should be failure
End


It 'does not contain whitespace'
Skip if "fd not available" missing_fd
# shellcheck disable=SC2046 # Intended word splitting
When run grep -En '\s+$' $(list_files --exclude='README.md')
The stdout should be blank
The stderr should be blank
The status should be failure
End


It 'does not contain tabs'
Skip if "fd not available" missing_fd
# shellcheck disable=SC2046 # Intended word splitting
When run grep -n $'\t' $(list_files --exclude='Makefile')
The stdout should be blank
The stderr should be blank
The status should be failure
End


It 'does not contain carriage returns'
Skip if "fd not available" missing_fd
# shellcheck disable=SC2046 # Intended word splitting
When run grep -n $'\r' $(list_files)
The stdout should be blank
The stderr should be blank
The status should be failure
End


It 'does not contain big files (1000 lines maximum)'
Skip if "fd not available" missing_fd
When run bash -c "list_files --exec wc -l {} \; | awk '\$1 > 1000 {print \$2}'"
The stdout should be blank
The stderr should be blank
The status should be success
End


It 'contains 15-25% of comments'
Skip if "scc not available" missing_scc
comments_ratio() { scc | grep -E ^Total | awk '{ printf "%.2f\n", ($5 * 100 / ($5 + $6)) }'; }
# shellcheck disable=SC2016 # Intended quoted variable
When run env COMMENTS_RATIO="$(comments_ratio)" bash -c 'echo "15.00 <= $COMMENTS_RATIO && $COMMENTS_RATIO <= 25.00" | bc -l'
The stdout should equal 1
The stderr should be blank
The status should be success
End


# It 'does not contain skipped tests'
# Pending
# When run grep -Rn xDescribe ./spec/ --exclude=code_spec.sh
# The stdout should be blank
# The stderr should be blank
# The status should be failure
# End


End

