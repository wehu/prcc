(use prcc)

(define p0 (<and> (char #\a)
                (one-of "abc")
                (eof)))
(display (parse-string "aad" p0))
(newline)
(display (parse-string "ab" p0))
(newline)
