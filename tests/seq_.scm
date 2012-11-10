(use prcc)

(use test)

(test-begin "seq_")

(define p0 (<and_> (char #\a)
                   (char #\a)))
(test (list "a" "a") (parse-string "a   abbb" p0))

(test-end "seq_")
