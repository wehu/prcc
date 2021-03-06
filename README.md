### Parser/Regex Combinator library for Chicken scheme

Prcc is a PEG-like combinator parser library by packrat parsing.
Inspired by ruby gem [rsec](https://github.com/luikore/rsec).

### Install

	chicken-install prcc

### Example

	(use prcc)

	(define parser
	  (<and>
	    (<@> (<s> "hello")
              (lambda (o) "hello "))
	    (<s> "world")
	    (eof)))

	(display (parse-string "helloworld" parser))
	(newline)

### Combinators

* `(char c)` : generate a parser that read a char
  * alias `<c>`
* `(seq parser [parsers ...])` : sequence parser
  * alias `<and>`
* `(sel parser [parsers ...])` : branch parser and ordered selected
  * alias `<or>`
* `(one? parser)` : appear 0 or 1 time
  * alias `<?>`
* `(rep parser)` : repeat 0 to infinite times
  * alias `<*>`
* `(rep+ parser)` : repeat 1 to infinite times
  * alias `<+>`
* `(pred parser0 parser1)` : lookahead predicate parser1
  * alias `<&>`
* `(pred! parser0 parser1)` : negative lookahead
  * alias `<&!>`
* `(eof)` : end of file
* `(act parser [succ-proc] [fail-proc])` : mutate parser and be used to process the output of a parser
  * alias `<@>`
* `(neg parser)` : take parser failure as pass
  * alias `<^>`
* `(regexp-parser string [chunk-size])`
  * alias `<r>`
* `(lazy parser)`
* `(cached parser)` : cache parser result
* helpers
  * `(str string)` : a string parser
      * alias `<s>`
  * `(one-of string)` : parse one of chars in string
  * `(join+ parser0 parser1)` : repeat parser0 with parser1 inserted
  * `(join+_ parser0 parser1 [skip: (<s*>)])` : skip spaces by default
  * `(ind seq-parser index)` : return the value that is indicated by index
      * alias `<#>`
  * `<w>` : word
  * `<w*>`
  * `<w+>`
  * `<space>` : space
  * `<s*>`
  * `<s+>`
  * `(rep_ parser [skip: (<s*>)])` : skip spaces by default
     * alias `<*_>`
  * `(rep+_ parser [skip: (<s*>)])` : skip spaces by default
     * alias `<+_>`
  * `(seq_ parser [parser...] [skip: (<s*>)])` : skip spaces by default
     * alias `<and_>`
  * `(even parser)` : return even elements of sequence
  * `(odd parser)` : return odd elements of sequence
* `(parse-file filename parser [cache])` : by default, no cache
* `(parse-string string parser [cache])`
* `(parse-port port parser [cache])`

