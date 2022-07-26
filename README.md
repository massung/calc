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

TODO: show asciinema to record examples

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
cut -d ',' -f2 values.txt | calc '_ deg/s : rev/min'


# or just run an interactive session
calc
1 + 1
_ V * 3 A : W


# and much, much more...
```


### Parameterized from stdin

```bash
calc '_ ft + 4 in'
1
16.00 in
2
28.00 in
<ctrl-d>

calc '_ psi : Pa' < values.txt
572265.00 Pa
393001.27 Pa
441264.58 Pa
530896.45 Pa
248211.33 Pa
137895.18 Pa
```

### Multiple parameters

```bash
calc '_ * 100 / (_ + _)' 43 78 3
53.09
```

### Interactive mode

```bash
calc
>> (10 W to J/s) * 3 hr
== 108000.00 J
>> _ / 4
== 27000.00 J
>> <ctrl-d>
```
