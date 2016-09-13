(use prcc)

(use test)

(test-begin "act")

(define p0 (<and> (char #\a)
                (<@> (one-of "abc")
                  (lambda (o) "ooo"))
                (<or> (<s> "abc")
                      (<s> "c"))
                (eof)))
(test (list "a" "ooo" "c" "") (parse-string "abc" p0))

(define p1 (<and> (char #\a)
                (<@> (one-of "abc")
                  (lambda (o) "ooo")
                  (lambda (o) "e"))
                ))
(test (list "a" "e") (parse-string "ae" p1))

(use data-structures)

(define p2 (act (char #\x) identity (constantly 1)))

(test 1 (parse-string "" p2))

(test-end "act")
