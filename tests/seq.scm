(use prcc)

(define p0 (<and> (char #\a)
                  (char #\a)
                  (<and> (<s> "bbb"))))
(display (parse-string "aabbb" p0))
(newline)

