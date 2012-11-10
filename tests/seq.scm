(use prcc)

(use test)

(test-begin "seq")

(define p0 (<and> (char #\a)
                  (char #\a)
                  (<and> (<s> "bbb"))))
(test (list "a" "a" (list "bbb")) (parse-string "aabbb" p0))

(test-end "seq")
