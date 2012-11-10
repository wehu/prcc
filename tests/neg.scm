(use prcc)

(use test)

(test-begin "neg")

(define p0 (<^> (char #\a)))
(test "b" (parse-string "b" p0))

(define p1 (<and>
              (<s> "abc")
              (<^> (<and> (char #\a)
                          (char #\c)))
              (<s> "ef")))
(test-assert (not (parse-string "abcacef" p1)))

(define p2 (<and>
              (<s> "abc")
              (<^> (<and> (char #\a)
                          (char #\c)))
              (<s> "ef")))
(test (list "abc" "ad" "ef") (parse-string "abcadef" p2))

(test-end "neg")
