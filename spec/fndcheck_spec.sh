#!/bin/bash
Describe 'sharp'
Describe 'fndcheck'

It 'reports error when symbols have errorful characters'
When run bash -c './bin/sharp fndcheck ./metatest/fndcheck/errorful_characters.fnd'
The stdout should include "*WARNING* (Parser): character found after backslash is not meaningful - '\#'"
The stderr should be blank
The status should be success
End

It 'reports lineread errors'
When run bash -c './bin/sharp fndcheck ./metatest/fndcheck/warning.fnd'
The stdout should include '*WARNING* Error when reading .fnd file'
The stderr should include '*Error* lineread/read: syntax error encountered in input'
The status should be failure
End

It 'reports nothing for valid .fnd file'
When run bash -c './bin/sharp fndcheck ./metatest/fndcheck/valid.fnd'
The stdout should be blank
The stderr should be blank
The status should be success
End


End
End

