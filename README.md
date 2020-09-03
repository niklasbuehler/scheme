Write Yourself a Scheme in 48 Hours
===

Just my own implementation (and extension?) of the [Write Yourself a Scheme in 48 Hours][tutorial] tutorial.

## Exercises
### Exercises 1.1
[ ] Rewrite `parseNumber` without `liftM`
[X] Parsing `\"` ([R5RS compliance](https://schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.3.5))
[X] Parse other escaped characters (`\n`, `\r`, `\t`, `\\` in strings
[X] Parse [numbers of different bases](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.2.4)
[X] Parse [character literals](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.3.4)
[X] Parse [Reals](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.2.4)
[X] Parse the [full numeric tower](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.2.1)

### Exercises 1.2
[X] Support [backquote syntactic sugar](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-7.html#%_sec_4.2.6)
[X] Support [vectors](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.3.6)
[ ] Left-factor the grammar instead of using the try combinator

## TODO
- Parse escaped characters, even when they're not part of a string

[tutorial]: https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours
