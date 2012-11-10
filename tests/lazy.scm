(use prcc)

(use test)

(test-begin "lazy")

(define p0 (join+ (char #\a)
                 (lazy p1)))
(define p1 (char #\b))

(test (list "a" "b" "a" "b" "a") (parse-string "ababa" p0))


(test-end "lazy")
