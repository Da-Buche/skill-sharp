<!--
theme: gaia
class: invert two-columns
auto-scaling: true
-->

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
- Code formatter
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
### Advanced Linter

#### Waiver

#### Detect unused local functions

#### Detect superseded functions

#### Hints


---
### Formatter

<!-- TODO -->

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

![bg right fit](https://github.com/Da-Buche/skill-sharp/blob/main/pictures/README/SKILL_Finder.png?raw=true)


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
