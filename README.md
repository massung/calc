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

## Example Usages

TODO: show asciinema to record examples

### Simple expressions

```bash
calc '(1+2)/3*4'
2.25
```

### Unit conversions

```bash
calc '10 ft : m'
3.05 m

calc '3 N to lb ft/min^2'
78116.54 ft lb/min^2
```

### Automatic unit conversions

```bash
calc '5 TB * 0.03/GB'
153.60

calc '2 ft + 3in'
15 in
```

### Parameterized expressions

```bash
calc '_ c : in^3' 1.5
20.81 in^3
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
