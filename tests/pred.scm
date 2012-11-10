(use prcc)

(use test)

(test-begin "pred")

(define p0 (<&> (char #\a)
                (<s> "bb")))
(test "a" (parse-string "abb" p0))

(test-assert (not (parse-string "ac" p0)))

(test-end "pred")
