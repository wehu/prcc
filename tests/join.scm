(use prcc)

(use test)

(test-begin "join+")

(define p0 (join+ (char #\a)
                (one-of "bc")))
(test (list "a" "b" "a" "c" "a") (parse-string "abaca" p0))

(test (list "a") (parse-string "a" p0))

(test (list "a" "b" "a") (parse-string "abac" p0))

(test-end "join+")
