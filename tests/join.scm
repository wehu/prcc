(use prcc)

(use test)

(test-begin "join+")

(define p0 (join+ (char #\a)
                (one-of "bc")))
(test (list "a" "b" "a" "c" "a") (parse-string "abaca" p0))

(test (list "a") (parse-string "a" p0))

(test (list "a" "b" "a") (parse-string "abac" p0))

(test-end "join+")

(test-begin "join+_")

(define p0 (join+_ (char #\a)
                (one-of "bc")))
(test (list "a" "b" "a" "c" "a") (parse-string "a b a  c a" p0))

(test (list "a") (parse-string "a" p0))

(define p1 (join+_ (char #\a)
                (one-of "bc")
                skip: (<c> #\,)))
(test (list "a" "b" "a" "c" "a") (parse-string "a,b,a,c,a" p1))


(test-end "join+_")


