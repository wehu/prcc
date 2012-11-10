(use prcc)

(use test)

(test-begin "pred!")

(define p0 (<&!> (char #\a)
                (<s> "bb")))
(test-assert (not (parse-string "abb" p0)))

(test "a" (parse-string "ac" p0))

(test-end "pred!")
