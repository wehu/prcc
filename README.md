### Prcc

Prcc is a PEG-like combinator parser library by packrat parsing


### Install

	git clone https://github.com/wehu/prcc.git
	cd prcc
	chicken-install

### Example

	(use prcc)

	(define parser
	  (seq
	    (act (str "hello")
              (lambda (o) "hello "))
	    (str "world")
	    (eof)))

	(display (parse-string "helloworld" parser))
	(newline)

### Combinators

* (char c) : generate a parser that read a char
* (seq parser [parsers ...]) : sequence parser
* (sel parser [parsers ...]) : branch parser and ordered selected
* (one? parser) : appear 0 or 1 time
* (rep parser) : repeat 0 to infinite times
* (rep+ parser) : repeat 1 to infinite times
* (pred parser0 parser1) : lookahead predicate parser1
* (pred! parser0 parser1) : negative lookahead
* (eof) : end of file
* (str string) : a string parser
* (one-of string) : parse one of chars in string
* (join parser0 parser1) : repeat parser0 with parser1 inserted
* (act parser [succ proc] [fail proc]) : mutate parser and be used to process the output of a parser
* (ind seq-parser index) : return the value that is indicated by index
* (parse-file filename parser)
* (parse-string string parser)
* (parse-port port parser)
