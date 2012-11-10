(use prcc)

(define p0 (<^> (char #\a)))
(display (parse-string "b" p0))
(newline)

(define p1 (<and>
              (<s> "abc")
              (<^> (<and> (char #\a)
                          (char #\c)))
              (<s> "ef")))
(display (parse-string "abcacef" p1))
(newline)

(define p2 (<and>
              (<s> "abc")
              (<^> (<and> (char #\a)
                          (char #\c)))
              (<s> "ef")))
(display (parse-string "abcadef" p2))
(newline)
