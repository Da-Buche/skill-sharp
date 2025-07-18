#!/bin/bash
Describe 'bash'

## Requirements

shellcheck_missing()  {
  ## Return true if shellcheck is missing, false otherwise
  ! shellcheck --version >/dev/null 2>&1;
}

shellcheck_outdated() {
  ## Return true if shellcheck is outdated (i.e. lower than 0.10.0), false otherwise
  { shellcheck --version | grep -Eo '[0-9]+\..*' ;
    echo '0.10.0' ;
  } | sort --version-sort --check
}

get_bash_files() {
  ## List all shell files in repo
  #shellcheck disable=SC2156
  find . -type f -not -iwholename '*/.git/*' -exec bash -c 'file -b "{}" | grep -q "Bourne-Again shell" && echo {}' \;
}

export -f get_bash_files

It "passes \`shellcheck\`"
Skip if "shellcheck not available"        shellcheck_missing
Skip if "shellcheck is older than 0.10.0" shellcheck_outdated
When run bash -c "get_bash_files | xargs shellcheck --format=gcc"
The stdout should be blank
The stderr should be blank
The status should be success
End


End

