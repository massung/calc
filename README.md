# CALC - a CLI calculator with support for units

Example usages:

```bash
$ # you can use it as a simple calculator
$ calc '(1+2)*3/4'
2.25

$ # it can alter output different precision
$ calc -p5 '7/13'
0.53846

$ # it can handle units
$ calc '5 mi/hr * 5 hr'
25.00 mi

$ # it can perform unit conversions
$ calc '5 mi/hr : m/s'
2.24 m/s

$ # it does implicit unit conversions
$ calc '2 gal + 3 c'
35.00 c

$ # it can use placeholders supplied by arguments
$ calc '_ J : BTU' 10
9.48e-3 BTU

$ # which allows piping large sets of data to it
$ shuf -i 1-1000 -n 5 | calc '_ ha to acre'
1828.58 acre
1670.43 acre
274.29 acre
165.56 acre
1371.43 acre

$ # or you can supply them via stdin yourself
$ calc '_ L to in^3'
1
61.02 in^3
2
122.05 in^3
3
183.07 in^3
<ctrl-d>

$ # supplied arguments can contain units
$ calc '_ m'
1
1.00 m
2 ft
0.61 m
<ctrl-d>

$ # it even has support for an interactive mode
$ calc -i
>> (10 W to J/s) * 3 hr
== 108000.00 J
>> _ / 4
== 27000.00 J
>> <ctrl-d>
```
