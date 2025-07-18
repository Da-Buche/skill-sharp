;; Example of Lint rules for custom macros

;; The following should report Lint warnings as variables are not used

(progn

  (@if (getShellEnvVar "IF_VARIABLE")
    ?var var_if
    12)

  (@nif (getShellEnvVar "NIF_VARIABLE")
    ?var var_nif
    27)

  (@when (getShellEnvVar "WHEN_VARIABLE")
    ?var var_when
    42)

  );progn


