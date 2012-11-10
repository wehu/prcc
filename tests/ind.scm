(use prcc)

(use test)

(test-begin "ind")

(define p0 (<#> (<and> (char #\a)
                       (one-of "bc")) 0))
(test "a" (parse-string "abaca" p0))

(define p1 (<#> (<and> (char #\a)
                       (one-of "bc")) 1))
(test "b" (parse-string "abaca" p1))

(test-end "ind")
