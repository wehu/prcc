(use prcc)

(use test)

(test-begin "rep")

(define p0 (<*> (char #\a)))

(test (list "a" "a" "a" "a" "a") (parse-string "aaaaa" p0))

(test `() (parse-string "b" p0))

(test-end "rep")
