(use prcc)

(define p0 (<&> (char #\a)
                (<s> "bb")))
(display (parse-string "abb" p0))
(newline)
(display (parse-string "ac" p0))
(newline)
