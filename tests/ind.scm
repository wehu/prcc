(use prcc)

(define p0 (<#> (<and> (char #\a)
                       (one-of "bc")) 0))
(display (parse-string "abaca" p0))
(newline)

(define p1 (<#> (<and> (char #\a)
                       (one-of "bc")) 1))
(display (parse-string "abaca" p1))
(newline)

