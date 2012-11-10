(use prcc)

(define p0 (join (char #\a)
                 (lazy p1)))
(define p1 (char #\b))
(display (parse-string "ababa" p0))
(newline)
