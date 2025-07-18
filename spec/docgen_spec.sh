#!/bin/bash
Describe 'sharp'
Describe 'docgen'

Parameters:dynamic
  for file in ./metatest/docgen/*.il ; do
    %data "${file}" "${file%.il}.fnd"
  done
  for file in ./metatest/docgen/*.ils ; do
    %data "${file}" "${file%.ils}.fnd"
  done
  for file in ./metatest/docgen/*.scm ; do
    %data "${file}" "${file%.scm}.fnd"
  done
End

It "generates valid doc for $1"
When run bash -c "SKILL_SHARP_BEFORE_COMMAND='(load \\\"./skill/loader.scm\\\")' ./bin/sharp docgen $1 | diff - $2"
The stdout should be blank
The stderr should be blank
The status should be success
End

End
End

