(@test
  ?fun '@bash

  (@assertion
    ?doc "Check stdout, stderr and exit status are well returned."
    (@bash "echo 12 ; >&2 echo 27 ; exit 42")
    ?out '("12\n" "27\n" 42)
    )

  (@assertion
    ?doc "Check special characters are well taken in account"
    (@bash "printf '%-10s\n\"Second line\".\n\n' word")
    ?out '("word      \n\"Second line\".\n\n" "" 0)
    )

  );test
