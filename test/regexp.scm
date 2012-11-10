(use prcc)

(define p0 (<and> (<r> "[abc]a?(cc)")
                  (<s> "abc")))
(display (parse-string "bccabc" p0))
(newline)

