(use prcc)

(use test)


(test-begin "rep+")

(define p0 (<+> (char #\a)))

(test (list "a" "a" "a" "a" "a") (parse-string "aaaaa" p0))

(test-assert (not (parse-string "b" p0)))

(test-end "rep+")

(test-begin "rep+_")

(define p0 (<+_> (char #\a)))

(test (list "a" "a" "a" "a" "a") (parse-string "aa  a aa" p0))

(test-assert (not (parse-string "b a" p0)))

(define p1 (<+_> (char #\a) skip: (<c> #\,)))

(test (list "a" "a" "a" "a" "a") (parse-string "a,a,a,a,a" p1))

(test-assert (not (parse-string "b a" p1)))

(test-end "rep+_")
