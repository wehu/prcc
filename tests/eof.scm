(use prcc)

(use test)

(test-begin "eof")

(define p0 (<and> (char #\a)
                (one-of "abc")
                (eof)))
(test-assert (not (parse-string "aad" p0)))

(test (list "a" "b" "") (parse-string "ab" p0))

(test-end "eof")
