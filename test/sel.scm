(use prcc)

(define p0 (<and> (char #\a)
                  (char #\a)
                  (<or> (<s> "bbb")
                         (<s> "ccc"))))
(display (parse-string "aabbb" p0))
(newline)
(display (parse-string "aabb" p0))
(newline)
(display (parse-string "aaccc" p0))
(newline)

