# Tournesol - a calculator for the CLI that understands units

A simple, scriptable, scalar expression and units calculator for the terminal. [Tournesol](https://www.tintin.com/en/characters/professor-calculus) - AKA Professor Calculus - is (quoting the linked page)...

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
function transferRate [storage; duration] = _/_ to MB/s
function areaOfCircle [length] = [pi] * _^2
```

And now you can load the script and use it in your calculations:

```bash
tn -f myfuncs.tn '[transferRate 10 GB; 20 min]'
tn -f myfuncs.tn '[areaOfCircle 2 ft]'
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

## Dimensions and Units

The following table of all (base) units and dimensions are understood by Tournesol. Units prefixed with a `-` may also be prefixed with the SI prefixes from atto- to exa- (see: [https://physics.nist.gov/cuu/Units/prefixes.html](https://physics.nist.gov/cuu/Units/prefixes.html)).

| Dimension | Base dimensions | Units
|-|-|-
| angle | `[angle]` | rad, grad, deg, rev, turn
| area | `[length^2]` | ha, acre
| duration | `[duration]` | -s, min, hr, day
| capacitance | `[current^2;duration^4;length^-2;mass^-1]` | -F
| charge | `[current;duration]` | -C
| current | `[current]` | -A
| energy | `[mass;length^2;duration^-2]` | -J, -eV, BTU, thm
| force | `[mass;length;duration^-2]` | -N, lbf, pond
| frequency | `[length^-1]` | -hz
| length | `[length]` | -m, mil, in, h, ft, yd, ftm, ch, fur, mi, lea, cable, nmi, link, rod, pc, au, ly
| mass | `[mass]` | -g, oz, lb, st, cwt, t
| power | `[mass;length^2;duration^-3]` | -W, hp
| pressure | `[mass;length^-1;duration^-2]` | -Pa, psi, bar
| resistance | `[mass;length^2;current^-4;duration^-3]` | -O
| speed | `[length;duration^-1]` | kph, mph, kn
| storage | `[storage]` | -B, -b
| voltage | `[mass;length^2;current^-1;duration^-3]` | -V
| volume | `[length^3]` | -L, tsp, tbp, floz, c, pt, qt, gal

_NOTE: Storage units (byte and bit) use base-2 SI prefixes (e.g. MB - megabyte - is 1048576 bytes, not 1000000)._

## Built-in Functions

The following functions are built-in and available for use:

```bash
# -------------------------------------------------
# Constants functions

function pi []

# -------------------------------------------------
# Trigometric functions

function sin [angle]
function cos [angle]
function tan [angle]
function sinh [angle]
function cosh [angle]
function tanh [angle]
function asin [none]
function acos [none]
function atan [none]
function asinh [none]
function acosh [none]
function atanh [none]

# -------------------------------------------------
# Math functions

function sqrt [any]
function log [none]
function exp [none]

# -------------------------------------------------
# Geometry functions

function areaOfCircle [length]
function areaOfRect [length; length]

# -------------------------------------------------
# Storage functions

function transferRate [storage; duration]
```
