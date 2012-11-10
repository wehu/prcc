(use prcc)

(define p0 (<and> (char #\a)
                (<@> (one-of "abc")
                  (lambda (o) "ooo"))
                (eof)))
(display (parse-string "ab" p0))
(newline)
