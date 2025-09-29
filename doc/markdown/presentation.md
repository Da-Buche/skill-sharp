---
marp: true
theme: gaia
class: invert two-columns
auto-scaling: true
---

<style>
section::after {
  content: attr(data-marpit-pagination) ' / ' attr(data-marpit-pagination-total);
}

section.two-columns ul {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 1rem;
}
</style>


# SKILL#

## Cadence SKILL++ Enhanced Framework

<br>
<br>

**Targeted Audience :** Anyone who writes SKILL

<br>
<br>

###### Created by [Aurélien Buchet](https://github.com/Da-Buche)

###### Open Source Repository on [GitHub](https://github.com/Da-Buche/skill-sharp)

---
<!-- 
_class: invert lead
_paginate: skip
-->

## Introduction


---
<!-- 
paginate: true
-->

### Out-of-the-Box Tools

- Advanced Linter
- Code formatter [unfinished]
- Enhanced Finder

<br>

### Advanced SKILL++ Features

- Unit-Testing Framework
- Documentation Generation
- On-Demand Type-Checking
- Fully Tested API


---
## Requirements

### To Use

Virtuoso with `$CDS_INST_DIR` defined

### To Develop

- [GNU Make](https://github.com/mirror/make)
- [scc](https://github.com/boyter/scc) 
- [shellcheck](https://github.com/koalaman/shellcheck) ≥ 0.10.0
- [fd](https://github.com/sharkdp/fd)
- [shellspec](https://github.com/shellspec/shellspec) ≥ 0.28.1

---
## Standalone Usage

`$SKILL_SHARP_ROOT/bin/sharp` commands:

| Command         | Documentation                                   |
|-----------------|-------------------------------------------------|
| `sharp help`    | Display available commands and arguments.       |
| `sharp lint`    | Run advanced Linter on provided files.          |
| `sharp test`    | Load  files and print test report.              |
| `sharp globals` | Load  files and report global definitions.      |
| `sharp docgen`  | Load files and print associated `.fnd` content. |


---
## Usage Inside Virtuoso

<br>

In the CIW or in `.cdsinit` to enable all SKILL# features:

`(load (simplifyFilename "$SKILL_SHARP_ROOT/skill/loader.scm"))`

<br>
<br>
<br>

*`$SKILL_SHARP_ROOT` should point to the installation root*


---
<!-- 
_class: invert lead
_paginate: skip
-->

## Features

---
### Rules for native Linter

- Waiver
  ```lisp
  (progn "NO_LINT" ...)
  {"NO_LINT" ...}
  ```

- Hints
  ```lisp
  (car (setof ...)) -> (car (exists ...))
  ```


- Missing docstrings

---
### Custom SKILL++ Linter

#### Detect unused & superseded local functions

```lisp
(let ()
  (defun custom_fun0 ( @rest _ ) "dummy" nil)
  (defun custom_fun0 ( @rest _ ) "dummy" nil)
  )
```

```lisp
WARNING DEFUN_SUPERSEDE at line 3   - `defun` variable custom_fun0 is superseded
WARNING LET_UNUSED at line 1   - `let` variable custom_fun0 is unused
```

Line numbers are reported inside all forms except macros
(only in Lisp syntax for now)

---
### Custom SKILL++ Linter

#### Syntax forms support

```lisp
;; This is valid SKILL but reported by Lint
(defun print_and_return args (println args) args)
(lambda _ t)
```


---
### Unit-Testing Framework

```scheme

(defun print_hello_world_and_return_12 (name "t")
  "Print 'Hello World!' said by NAME and return 12."
  (info "%s says 'Hello World! to poport'\n" name)
  (warn "%s says 'Hello World! to woport'\n" name)
  12)

(@test
  ?fun 'print_hello_world_and_return_12
  (@assertion
    ?doc "Works with John"
    (print_hello_world_and_return_12 "John")
    ?out 12
    ?info "John says 'Hello World!' to poport"
    ?warn "John says 'Hello World!' to woport"
    )
  (@assertion
    ?doc "Fails with a symbol"
    (print_hello_world_and_return_12 'John_as_a_symbol)
    ?error "print_hello_world_and_return_12: argument #1 should be a string"
    )
  )
```

---
```lisp 
;; ./failing_assertion.ils
(@test
   ?fun 'plus
   ?doc "Test with failing assertion"
   (@assertion 12+27 ?out 42)
   )

> (@test_run_all ?files '("./failing_assertion.ils"))
Failed @test("primop plus")[stdobj@0x2703d080] from *ciwInPort*

Failures when running @assertion(0)[stdobj@0x2703d098]:
  (plus 12 27)

Different output:
Expected: █42█
Got     : █39█

Total tests: 2
 - skipped tests: 0
 - passed  tests: 0
 - failed  tests: 1

Total assertions: 2
 - skipped assertions: 0
 - passed  assertions: 0
 - failed  assertions: 1

FAIL
```



<!--
---
### Formatter

 TODO -->

---
### Type-Checking
#### On-Demand

```scheme
(@fun join
  ( ( char    ?type symbol|string         ) ; char is a symbol or a string
    @rest
    ( strings ?type ( symbol|string ... ) ) ; strings is a list of symbols or strings
    )
  ?doc "Join STRINGS using CHAR as junction character."
  ?out string                               ; output is a string
  (buildString strings char)
  )
```

To enable strict type-checking:  
Set `$SKILL_SHARP_STRICT_TYPE_CHECKING` to TRUE


---
### Type-Checking
#### Always Disabled

```scheme
;; Type-checking always disabled
;; (Still useful for documentation)

(@fun convert_to_string
  ( ( name ?type symbol )
    )
  ?doc    "Convert NAME to a string and return it."
  ?out    string
  ?strict nil                               ; Type-checking is disabled here
  (strcat name)
  )
```

---
### Type-Checking
#### Always Enabled

```scheme
;; Type-checking always enabled
;; (Useful for top functions)

(@fun double
  ( ( num ?type float|integer )
    )
  ?doc    "Return the double of NUM as a float."
  ?out    float
  ?strict t                                 ; Type-checking is forced here
  ( num * 2.0 )
  )
```

---
<!-- 
_class: invert
_paginate: false
-->

### Documentation Generation
### Enhanced Finder

- Search in name, description, ...
- Match PCREs
- Case-sensitive when upper case
- Restrict to name, description, ...

<!-- TODO: Fix link to dev branch -->
![bg right fit](https://github.com/Da-Buche/skill-sharp/blob/dev/pictures/README/SKILL_Finder.png?raw=true)


---
<!-- 
_class: invert lead
_paginate: skip
-->

## Design Patterns / Macros

---
### F-Strings

```scheme
;; Values are placed where they should be evaluated:
(@str "The result of 12 plus 27 is {12+27}.)")

(@str "
The current shell is {(getShellEnvVar \"SHELL\")}.
The current time is {(getCurrentTime)}.
")

;; `printf` formatting is supported
pi = (acos -1)
(@str "
This is pi with four decimals: {pi%0.4f}. \n\
The default is {pi}. \n\
")
```

---
### Letf

```scheme
;; `simplifyFilename` breaks if `(rexMagic)` is nil.
(progn (rexMagic nil) (simplifyFilename "$SHELL"))

;; `@letf` uses `setf` to set anything temporarily.
(@letf ( ( (rexMagic) t ) ) (simplifyFilename "$SHELL"))
(rexMagic) ; `(rexMagic)` value is still nil.

;; `@letf` works with any `setf` helper
(@letf ( ( (rexMagic)                         nil    )
         ( (getShellEnvVar "CUSTOM_VARIABLE") "TRUE" )
         ( (status optimizeTailCall)          t      )
         )
  (list (rexMagic) (getShellEnvVar "CUSTOM_VARIABLE") (status optimizeTailCall))
  )
```

---
### With
<!-- 
_paginate: skip
-->

```scheme
;; Filter INFO lines from log file (Ports are properly closed afterwards)
(@with ( ( in_port  (infile  "~/sandbox/log.txt"         ) )
         ( out_port (outfile "~/sandbox/filtered_log.txt") )
         )
  (let ( line )
    (while (gets line in_port)
      (unless (pcreMatchp "^INFO" line) (fprintf out_port "%s" line))
      )))
```


```lisp
(defmethod @in ( ( obj dbobject ) @rest _ ) "Nothing to do here" nil)

(defmethod @out ( ( obj dbobject ) @rest _ )
  "Context manager when releasing a dbobject."
  (if (equal "cellView" obj->objType) (dbClose obj)
    (error "@out - %N is not a supported type.")))
```

---
### Case

```lisp
(@case (css)->objType ; An ellipse is selected
  ( "polygon" (css)->points )
  ( "rect"    (destructuringBind ( ( x0 y0 ) ( x1 y1 ) ) (css)->bBox)
                (list x0:y0 x1:y0 x1:y1 x0:y1) )
  )
*Error* Value is not amongst valid cases ("polygon" "rect"): "ellipse"
```

```lisp
(@caseq 'd
  ( a 12 )
  ( b 27 )
  ( c 42 )
  )
*Error* Value is not amongst valid cases (a b c): 'd
```

