(use prcc)

(define p0 (join (char #\a)
                (one-of "bc")))
(display (parse-string "abaca" p0))
(newline)
(display (parse-string "a" p0))
(newline)
(display (parse-string "abac" p0))
(newline)