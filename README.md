### Parser/Regex Combinator library for Chicken scheme

Prcc is a PEG-like combinator parser library by packrat parsing


### Install

	git clone https://github.com/wehu/prcc.git
	cd prcc
	chicken-install

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
* `(act parser [succ proc] [fail proc])` : mutate parser and be used to process the output of a parser
  * alias `<@>`
* `(neg parser)` : take parser failure as pass
  * alias `<^>`
* `(regexp-parser string)`
  * alias `<r>`
* `(lazy parser)`
* helpers
  * `(str string)` : a string parser
    * alias `<s>`
  * `(one-of string)` : parse one of chars in string
  * `(join parser0 parser1)` : repeat parser0 with parser1 inserted
  * `(ind seq-parser index)` : return the value that is indicated by index
    * alias `<#>`
  * `<w>` : word
  * `<w*>`
  * `<w+>`
  * `<space>` : space
  * `<s*>`
  * `<s+>`
* `(parse-file filename parser)`
* `(parse-string string parser)`
* `(parse-port port parser)`

