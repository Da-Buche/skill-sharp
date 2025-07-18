#!/bin/bash
# shellcheck disable=SC2086,SC2126
Describe 'skill'


## =======================================================
## Utilities
## =======================================================

#shellcheck disable=SC2120
function get_skill_files() {
  ## List all SKILL files in repo
  find "${1-.}" -type f \( -name '*.il' -o -name '*.ils' -o -name '*.scm' \)
}

function comment_code_ratio() {
  ## Calculate comment over code lines ratio
  skill_files=$(get_skill_files)
  ## Find code lines (i.e. non-empty ones)
  total_lines=$(wc -l $skill_files | grep -Eo '^\s*[0-9]+\s+total$' | grep -Eo '[0-9]+')
  empty_lines=$(grep -Eh '^\s*$' $skill_files | wc -l)
  code_lines=$((total_lines - empty_lines))
  ## Find comment lines (filter commented code and inline comments)
  comment_lines=$(grep -Eh '^\s*;;' $skill_files | wc -l)
  doc_lines=$(grep -Pzo '\?doc[\s\n]+"[^"]*"' $skill_files | sed 's/\x0/\n/g' | wc -l)

  ## Avoid division by zero
  if [ $code_lines -eq 0 ]; then
    >&2 echo 'Error: No SKILL code lines detected...'
    return 1
  fi

  echo "scale=2; ( ( $comment_lines + $doc_lines ) * 100 ) / $code_lines" | bc
}

function geq() {
  ## Return true if first argument is greater than or equal to second argument
  [ "$(echo "$1 >= $2" | bc)" -eq 1 ]
}

export -f get_skill_files
export -f geq


## =======================================================
## Actual tests
## =======================================================

It 'has 15-25% comment lines'
#shellcheck disable=SC2016
When call env ratio="$(comment_code_ratio)" bash -c '{ geq $ratio 15 && geq 25 $ratio ; } || { printf "Comment/Code : ${ratio} %%\n" && exit 1 ; }'
The stdout should be blank
The stderr should be blank
The status should be success
End


It 'docstrings ends with dot'
End


It 'has no missing test files'
When run bash -c "get_skill_files skill | sed -e 's/^skill/test/g' -e 's:\(\.[^.]*\$\):_test\1:g' | xargs -I% bash -c 'test -f % || echo %'"
The stdout should be blank
The stderr should be blank
The status should be success
End


It 'has no extra test files'
When run bash -c "get_skill_files test | sed -e 's/^test/skill/g' -e 's:_test\(\.[^.]*\$\):\1:g' | xargs -I% bash -c 'test -f % || echo %'"
The stdout should be blank
The stderr should be blank
The status should be success
End


It 'each global definition is tested in associated test file'
End


End

