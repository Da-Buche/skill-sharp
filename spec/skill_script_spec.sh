#!/bin/bash
Describe 'skill_script'

It 'is executable'
  When run test -x ./metatest/script.scm
  The stdout should be blank
  The stderr should be blank
  The status should be success
End


## -------------------------------------------------------
## Usage
## -------------------------------------------------------

Describe 'usage'

  Parameters
    '-u'
    '--u'
    '-usage'
    '--usage'
  End

  It "prints usage with $1"
    When run bash -c "./metatest/script.scm $1"
    The stdout should be blank
    The stderr should equal 'Usage: script.scm [-n|--no-check] [-f|--format=FORMAT] [--name_with_underscore] [-q|--quick|--no-quick] [--verbose] -i|--input-file=FILE -o|--output-dir=DIR REQUIRED_ARG ANOTHER_REQUIRED_ARG [OPTIONAL_ARG] [ANOTHER_OPTIONAL_ARG] [REST_ARGS...]
Description of example script.'
    The status should be failure
  End

End


## -------------------------------------------------------
## Help
## -------------------------------------------------------

Describe 'help'

  Parameters
    '-h'
    '--h'
    '-help'
    '--help'
  End

  It "prints help with $1"
    When run bash -c "./metatest/script.scm $1"
    The stdout should be blank
    The stderr should equal 'Usage: script.scm [-n|--no-check] [-f|--format=FORMAT] [--name_with_underscore] [-q|--quick|--no-quick] [--verbose] -i|--input-file=FILE -o|--output-dir=DIR REQUIRED_ARG ANOTHER_REQUIRED_ARG [OPTIONAL_ARG] [ANOTHER_OPTIONAL_ARG] [REST_ARGS...]
Description of example script.

Arguments
      -i|--input-file=FILE  Path of the input file.
       -o|--output-dir=DIR  Name of the output directory.
              REQUIRED_ARG  First positional argument. This one is required.
      ANOTHER_REQUIRED_ARG  Second positional argument. This one is required.
            [OPTIONAL_ARG]  Third positional argument. This one is optional.  [default: ""]
    [ANOTHER_OPTIONAL_ARG]  Fourth positional argument. This one is optional. [default: ""]
            [REST_ARGS...]  Other positional arguments.                       [default: nil]

Options
           [-n|--no-check]  Enable checks.                                    [default: t]
      [-f|--format=FORMAT]  Format of the output.                             [default: "JSON"]
  [--name_with_underscore]  Dummy example.                                    [default: t]
                            With docstring over two lines.
   [-q|--quick|--no-quick]  Toggle quick mode.                                [default: nil]
               [--verbose]  Display more messages.                            [default: nil]

Callbacks
               [-h|--help]  Show script help and exit.
              [-u|--usage]  Show script usage and exit.
            [-v|--version]  Show script version and exit.'
    The status should be failure
  End

End


## -------------------------------------------------------
## Version
## -------------------------------------------------------

Describe 'version'

  Parameters
    '-v'
    '--v'
    '-version'
    '--version'
  End

  It "prints version with $1"
    When run bash -c "./metatest/script.scm $1"
    The stdout should equal 'script.scm 0.0.0'
    The stderr should be blank
    The status should be success
  End

End


## -------------------------------------------------------
## Other options
## -------------------------------------------------------

Describe 'options'

  It 'fails when provided an extra option'
    When run bash -c "./metatest/script.scm --missing-option -oOUT_DIR -iINPUT_FILE arg0 arg1"
    The stdout should be blank
    The stderr should equal '*Error* Not a valid option: --missing-option'
    The status should be success
  End

  It 'stores true'
    When run bash -c "./metatest/script.scm --verbose -oOUT_DIR -iINPUT_FILE arg0 arg1"
    The stdout should include 'name_with_underscore : t'
    The stdout should include 'verbose              : t'
    The stderr should be blank
    The status should be success
  End

  It 'stores false'
    When run bash -c "./metatest/script.scm --name_with_underscore -oOUT_DIR -iINPUT_FILE arg0 arg1"
    The stdout should include 'name_with_underscore : nil'
    The stdout should include 'verbose              : nil'
    The stderr should be blank
    The status should be success
  End

  It 'stores true & false'
    When run bash -c "./metatest/script.scm --verbose --name_with_underscore -oOUT_DIR -iINPUT_FILE arg0 arg1"
    The stdout should include 'name_with_underscore : nil'
    The stdout should include 'verbose              : t'
    The stderr should be blank
    The status should be success
  End

  It 'stores false using short key'
    When run bash -c "./metatest/script.scm -n -oOUT_DIR -iINPUT_FILE arg0 arg1"
    The stdout should include 'name_with_underscore : t'
    The stdout should include 'check                : nil'
    The stderr should be blank
    The status should be success
  End

  It 'stores false with long key'
    When run bash -c "./metatest/script.scm --no-check -oOUT_DIR -iINPUT_FILE arg0 arg1"
    The stdout should include 'name_with_underscore : t'
    The stdout should include 'check                : nil'
    The stderr should be blank
    The status should be success
  End

  It 'supports same option provided twice (short & short)'
    When run bash -c "./metatest/script.scm -n --name_with_underscore -n -oOUT_DIR -iINPUT_FILE arg0 arg1"
    The stdout should include 'name_with_underscore : nil'
    The stdout should include 'check                : nil'
    The stderr should be blank
    The status should be success
  End

  It 'supports same option provided twice (short & long)'
    When run bash -c "./metatest/script.scm -n --no-check --name_with_underscore -oOUT_DIR -iINPUT_FILE arg0 arg1"
    The stdout should include 'name_with_underscore : nil'
    The stdout should include 'check                : nil'
    The stderr should be blank
    The status should be success
  End


  It 'supports store_true option with opposite keys (short)'
    When run bash -c "./metatest/script.scm -q -oOUT_DIR -iINPUT_FILE arg0 arg1"
    The stdout should include 'name_with_underscore : t'
    The stdout should include 'quick                : t'
    The stderr should be blank
    The status should be success
  End

  It 'supports store_true option with opposite keys (long)'
    When run bash -c "./metatest/script.scm --quick -oOUT_DIR -iINPUT_FILE arg0 arg1"
    The stdout should include 'name_with_underscore : t'
    The stdout should include 'quick                : t'
    The stderr should be blank
    The status should be success
  End

  It 'supports store_true option with opposite keys (opposite)'
    When run bash -c "./metatest/script.scm --no-quick -oOUT_DIR -iINPUT_FILE arg0 arg1"
    The stdout should include 'name_with_underscore : t'
    The stdout should include 'quick                : nil'
    The stderr should be blank
    The status should be success
  End

  It 'supports store_true option with opposite keys (long, opposite)'
    When run bash -c "./metatest/script.scm --quick --no-quick -oOUT_DIR -iINPUT_FILE arg0 arg1"
    The stdout should include 'name_with_underscore : t'
    The stdout should include 'quick                : nil'
    The stderr should be blank
    The status should be success
  End

  It 'supports store_true option with opposite keys (opposite, long)'
    When run bash -c "./metatest/script.scm --no-quick --quick -oOUT_DIR -iINPUT_FILE arg0 arg1"
    The stdout should include 'name_with_underscore : t'
    The stdout should include 'quick                : t'
    The stderr should be blank
    The status should be success
  End

  It 'supports store_true option with opposite keys (long, opposite, short)'
    When run bash -c "./metatest/script.scm --quick --no-quick -q -oOUT_DIR -iINPUT_FILE arg0 arg1"
    The stdout should include 'name_with_underscore : t'
    The stdout should include 'quick                : t'
    The stderr should be blank
    The status should be success
  End

  It 'stores default value'
    When run bash -c "./metatest/script.scm --verbose -oOUT_DIR -iINPUT_FILE arg0 arg1"
    The stdout should include 'name_with_underscore : t'
    The stdout should include 'format               : "JSON"'
    The stdout should include 'verbose              : t'
    The stderr should be blank
    The status should be success
  End

  It 'stores specified value (short attached)'
    When run bash -c "./metatest/script.scm -fYAML -oOUT_DIR -iINPUT_FILE arg0 arg1"
    The stdout should include 'name_with_underscore : t'
    The stdout should include 'format               : "YAML"'
    The stderr should be blank
    The status should be success
  End

  It 'stores specified value (short attached with equal)'
    When run bash -c "./metatest/script.scm -f=PDF -oOUT_DIR -iINPUT_FILE arg0 arg1"
    The stdout should include 'name_with_underscore : t'
    The stdout should include 'format               : "PDF"'
    The stderr should be blank
    The status should be success
  End

  It 'stores specified value (short attached with two equal signs)'
    When run bash -c "./metatest/script.scm -f=PDF=2 -oOUT_DIR -iINPUT_FILE arg0 arg1"
    The stdout should include 'name_with_underscore : t'
    The stdout should include 'format               : "PDF=2"'
    The stderr should be blank
    The status should be success
  End


  It 'stores specified value (short separated with equal)'
    When run bash -c "./metatest/script.scm -f =TXT -oOUT_DIR -iINPUT_FILE arg0 arg1"
    The stdout should include 'name_with_underscore : t'
    The stdout should include 'format               : "=TXT"'
    The stderr should be blank
    The status should be success
  End

  It 'fails storing specified value (long attached without equal)'
    When run bash -c "./metatest/script.scm --formatYAML -oOUT_DIR -iINPUT_FILE arg0 arg1"
    The stdout should be blank
    The stderr should include 'Not a valid option: --formatYAML'
    The status should be success
  End

  It 'stores specified value (long attached with equal)'
    When run bash -c "./metatest/script.scm --format=PDF -oOUT_DIR -iINPUT_FILE arg0 arg1"
    The stdout should include 'name_with_underscore : t'
    The stdout should include 'format               : "PDF"'
    The stderr should be blank
    The status should be success
  End

  It 'stores specified value (long attached with two equal signs)'
    When run bash -c "./metatest/script.scm --format=PDF=2 -oOUT_DIR -iINPUT_FILE arg0 arg1"
    The stdout should include 'name_with_underscore : t'
    The stdout should include 'format               : "PDF=2"'
    The stderr should be blank
    The status should be success
  End

  It 'stores specified value (long separated with equal)'
    When run bash -c "./metatest/script.scm --format =TXT -oOUT_DIR -iINPUT_FILE arg0 arg1"
    The stdout should include 'name_with_underscore : t'
    The stdout should include 'format               : "=TXT"'
    The stderr should be blank
    The status should be success
  End

  It 'stores required positional arguments'
    When run bash -c "./metatest/script.scm first_argument second_argument -oOUT_DIR -iINPUT_FILE"
    The stdout should include 'name_with_underscore : t'
    The stdout should include 'required_arg         : "first_argument"'
    The stdout should include 'another_required_arg : "second_argument"'
    The stderr should be blank
    The status should be success
  End

  It 'stores optional positional arguments'
    When run bash -c "./metatest/script.scm first_argument second_argument third_argument -oOUT_DIR -iINPUT_FILE"
    The stdout should include 'name_with_underscore : t'
    The stdout should include 'required_arg         : "first_argument"'
    The stdout should include 'another_required_arg : "second_argument"'
    The stdout should include 'optional_arg         : "third_argument"'
    The stderr should be blank
    The status should be success
  End

  It 'stores rest argument'
    When run bash -c "./metatest/script.scm first_argument second_argument third_argument fourth_argument fifth_argument -oOUT_DIR -iINPUT_FILE"
    The stdout should include 'name_with_underscore : t'
    The stdout should include 'required_arg         : "first_argument"'
    The stdout should include 'another_required_arg : "second_argument"'
    The stdout should include 'optional_arg         : "third_argument"'
    The stdout should include 'another_optional_arg : "fourth_argument"'
    The stdout should include 'rest_args            : ("fifth_argument")'
    The stderr should be blank
    The status should be success
  End

  It 'supports "--" to separate positional arguments starting with "-" from options'
  When run bash -c "./metatest/script.scm -iin -oout -- first -- second -iIN"
    The stdout should include 'name_with_underscore : t'
    The stdout should include 'required_arg         : "first"'
    The stdout should include 'another_required_arg : "--"'
    The stdout should include 'optional_arg         : "second"'
    The stdout should include 'another_optional_arg : "-iIN"'
    The stderr should be blank
    The status should be success
  End

  It 'stores proper values for all options and arguments (Example 0)'
    When run bash -c "./metatest/script.scm --verbose -nqfYAML -i=in -o out --no-quick required0 required1 optional0 also_optional1 rest0 rest1 rest2 "
    The stdout should equal 'another_optional_arg : "also_optional1"
another_required_arg : "required1"
check                : nil
format               : "YAML"
help                 : __unbound__
input-file           : "in"
name_with_underscore : t
optional_arg         : "optional0"
output-dir           : "out"
quick                : nil
required_arg         : "required0"
rest_args            : ("rest0" "rest1" "rest2")
usage                : __unbound__
verbose              : t
version              : __unbound__'
    The stderr should be blank
    The status should be success
  End

  It 'stores proper values for all options and arguments (Example 1)'
    When run bash -c "./metatest/script.scm --output-dir out_dir --verbose --format=PDF --input-file in_file -q req also_req"
    The stdout should equal 'another_optional_arg : ""
another_required_arg : "also_req"
check                : t
format               : "PDF"
help                 : __unbound__
input-file           : "in_file"
name_with_underscore : t
optional_arg         : ""
output-dir           : "out_dir"
quick                : t
required_arg         : "req"
rest_args            : nil
usage                : __unbound__
verbose              : t
version              : __unbound__'
    The stderr should be blank
    The status should be success
  End

End


End

