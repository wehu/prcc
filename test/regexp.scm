(use prcc)

(define p0 (<and> (<r> "[abc]a?(cc)\\w")
                  (<s> "abc")))
(display (parse-string "bccjabc" p0))
(newline)

