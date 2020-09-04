Write Yourself a Scheme in 48 Hours
===

Just my own implementation (and extension?) of the [Write Yourself a Scheme in 48 Hours][tutorial] tutorial.

## Exercises
### Exercises 2.1
- [ ] Rewrite `parseNumber` without using `liftM`
- [x] Parsing `\"` ([R5RS compliance](https://schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.3.5))
- [x] Parse other escaped characters (`\n`, `\r`, `\t`, `\\`) in strings
- [x] Parse [numbers of different bases](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.2.4)
- [x] Parse [character literals](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.3.4)
- [x] Parse [Reals](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.2.4)
- [x] Parse the [full numeric tower](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.2.1)

### Exercises 2.2
- [x] Support [backquote syntactic sugar](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-7.html#%_sec_4.2.6)
- [x] Support [vectors](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.3.6)
- [ ] Left-factor the grammar instead of using the try combinator

### Exercises 3
- [X] Add Primitives for [type-testing](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.3): `symbol?`, `string?`, `number?` etc.
- [X] Remove _weak typing_ from `unpackNum`, s.t. it always returns 0 if the value is not a number
- [X] Add [symbol-handling functions](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.3.3)

## TODO
- [ ] Parse escaped characters, even when they're not part of a string
- [ ] Eval pattern for `quasiquote` and `unquote-splicing`

[tutorial]: https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours
