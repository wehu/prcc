(use prcc)

(define p0 (<and> (<r> "[abc]a?(cc)\\w")
                  (<s> "abc")))
(display (parse-string "bccjabc" p0))
(newline)

(define p1 (<and> (<w+>)
                  (eof)))
(display (parse-string "bccjabc" p1))
(newline)

(define p2 (<and> (<s*>)))
(display (parse-string " \n\t     " p2))
(newline)
