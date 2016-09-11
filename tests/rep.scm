(use prcc)

(use test)

(test-begin "rep")

(define p0 (<*> (char #\a)))

(test (list "a" "a" "a" "a" "a") (parse-string "aaaaa" p0))

(test `() (parse-string "b" p0))

(test-end "rep")

(test-begin "rep_")

(define p0 (<*_> (char #\a)))

(test (list "a" "a" "a" "a" "a") (parse-string "a aa a a" p0))

(test `() (parse-string "b" p0))

(define p1 (<*_> (char #\a) skip: (char #\,)))

(test (list "a" "a" "a" "a" "a") (parse-string "a,a,a,a,a" p1))

(test `() (parse-string "b" p1))

(test-end "rep_")
