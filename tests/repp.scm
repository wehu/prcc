(use prcc)

(use test)


(test-begin "repp")

(define p0 (<+> (char #\a)))

(test (list "a" "a" "a" "a" "a") (parse-string "aaaaa" p0))

(test-assert (not (parse-string "b" p0)))

(test-end "repp")
