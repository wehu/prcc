(use prcc)

(define p0 (<?> (char #\a)))
(display (parse-string "a" p0))
(newline)
(display (parse-string "b" p0))
(newline)
