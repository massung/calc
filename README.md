# Tournesol - a calculator for the CLI that understands units

A simple, scriptable, scalar expression and units calculator for the terminal.

[Tournesol](https://www.tintin.com/en/characters/professor-calculus) - AKA Professor Calculus - is (quoting the linked page)...

> ...very absent-minded, hard of hearing, intuitive and very sentimental. He is capable of the most unexpected, and sometimes really weird, connections with reality, by simply using his pendulum. First and foremost he is a fine handyman, then a clever inventor. Calculus is intrigued by everything, including botany, physics, electronics and dowsing. He has all the traits of a scientist who is determined to make his ideas work. Self-assurance, and obstinacy verging on irritability.

![](https://cdn001.tintin.com/public/tintin/img/static/professor-calculus/tournesol-calculus.jpg)

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
tn '1+2'

# more complex expressions with unit conversions
tn '300 W * 2 hr : BTU'
tn '100 mL + 1 c to in^3'

# make use of placeholder arguments
tn '_ / _ in' '3 yd^2' 4

# evaluate piped, delimited values
tn '_ deg/s : rev/min' < values.txt

# call functions that understand units
tn '[sin 45 deg] * [cos [pi] / 4]'
```

## Scripts and Custom Functions

You can define your own functions in scripts:

```bash
# myfuncs.tn
function transferRate [MB; s] = _/_
```

And now you can load the script and use it in your calculations:

```bash
tn -f myfuncs.tn '[transferRate 10 GB; 20 min]'
```

## Interactive Mode

It's also possible to simply run in interactive mode:

```
$ tn
Tournesol v1.0.0, (c) Jeffrey Massung
>> 1 + 1
== 2.00
>> _ V * 0.5 A * 2 min
== 2.00 V A min
>> _ J
== 120.00 J
```
