(use prcc)

(define p0 (<and> (char #\a)
                (<@> (one-of "abc")
                  (lambda (o) "ooo"))
                (eof)))
(display (parse-string "ab" p0))
(newline)

(define p1 (<and> (char #\a)
                (<@> (one-of "abc")
                  (lambda (o) "ooo")
                  (lambda (o) (display "meeting \'e\'")))
                (eof)))
(display (parse-string "ae" p1))
(newline)

