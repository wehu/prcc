(use prcc)

(use test)


(test-begin "rep+")

(define p0 (<+> (char #\a)))

(test (list "a" "a" "a" "a" "a") (parse-string "aaaaa" p0))

(test-assert (not (parse-string "b" p0)))

(test-end "rep+")

(test-begin "rep+_")

(define p0 (<+> (char #\a)))

(test (list "a" "a" "a" "a" "a") (parse-string "aa  a aa" p0))

(test-assert (not (parse-string "b a" p0)))

(test-end "rep+_")
