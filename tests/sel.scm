(use prcc)

(use test)

(test-begin "sel")

(define p0 (<and> (char #\a)
                  (char #\a)
                  (<or> (<s> "bbb")
                         (<s> "ccc"))))
(test (list "a" "a" "bbb") (parse-string "aabbb" p0))

(test-assert (not (parse-string "aabb" p0)))

(test (list "a" "a" "ccc") (parse-string "aaccc" p0))

(test-end "sel")
