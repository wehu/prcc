(use prcc)

(use test)

(test-begin "one?")

(define p0 (<?> (char #\a)))
(test "a" (parse-string "a" p0))

(test "" (parse-string "b" p0))

(test-end "one?")
