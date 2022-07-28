# CALC - a calculator for the CLI that understands units

A simple, scriptable, scalar expression and units calculator for the terminal.

## Setup

### Install Haskell and Stack

1. Install [ghcup](https://www.haskell.org/ghcup/)
2. Run `ghcup` to install [stack](https://docs.haskellstack.org/en/stable/README/)

### Build from Source

1. Clone the repository
2. From within the repository directory run `stack build` and/or `stack install`
3. Optionally ensure it's all good with `stack test`

## Examples

```bash
# simple expressions
calc '1+2'

# more complex expressions with unit conversions
calc '300 W * 2 hr : BTU'
calc '100 mL + 1 c to in^3'

# make use of placeholder arguments
calc '_ / _ in' '3 yd^2' 4

# evaluate piped, delimited values
calc '_ deg/s : rev/min' < values.txt

# call functions that understand units
calc '[sin 45 deg] * [cos [pi] / 4]'
```

## Scripts and Custom Functions

You can define your own functions in scripts:

```bash
# myfuncs.calc
function transferRate [MB; s] = _/_
```

And now you can load the script and use it in your calculations:

```bash
calc -f myfuncs.calc '[transferRate 10 GB; 20 min]'
```

## Interactive Mode

It's also possible to simply run in interactive mode:

```
$ calc
calc v1.0.0, (c) Jeffrey Massung
>> 1 + 1
== 2.00
>> _ V * 0.5 A * 2 min
== 2.00 V A min
>> _ J
== 120.00 J
```
