# CALC - a CLI calculator with support for units

A scalar expression and units calculator for the terminal.

```
$ calc -?
calc v1.0, (c) Jeffrey Massung

calc [OPTIONS] [EXPRESSION [ARGS...]]

Common flags:
  -p --precision=N
  -n --no-units
  -d --delimiter=SEP
  -? --help             Display help message
  -V --version          Print version information
     --numeric-version  Print just the version number

Scalar expression and units calculator
```

## Usages screencast



### Simple expressions

```bash

# calc is a simple calculator
calc '(1+2)/3*4'


# that understands units
calc '5 GB : Mb'


# and complex unit conversions
calc '10 W hr to BTU'
calc '100 mL to in^3'


# and can perform automatic unit conversions
calc '2 mi + 3 km'
calc '40 m^2/s + 2 acre/min'


# call built-in functions that also understand units
calc '[sin 45 deg]'


# use placeholders for inputs
calc '_ tbsp : floz' 10
calc '100 hz * _ m : mph' 13


# even multiple placeholders
calc '_ + _ * _' 1 2 3


# stream inputs from stdin
calc '_ t : kg'
1
2
3


# or pipe them
head -n 5 values.txt | cut -d ',' -f2 | calc '_ deg/s : rev/min'


# or just run an interactive session
calc
1 + 1
_ V * 3 A : W


# and much, much more...
```

