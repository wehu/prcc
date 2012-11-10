(use prcc)

(use test)

(test-begin "one-of")

(define p0 (<and> (char #\a)
                (one-of "abc")))
(test (list "a" "a") (parse-string "aa" p0))

(test (list "a" "b") (parse-string "ab" p0))

(test (list "a" "c") (parse-string "ac" p0))

(test-end "one-of")
