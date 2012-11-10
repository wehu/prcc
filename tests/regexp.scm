(use prcc)

(use test)

(test-begin "regexp")

(define p0 (<and> (<r> "[abc]a?(cc)\\w")
                  (<s> "abc")))
(test (list "bccj" "abc") (parse-string "bccjabc" p0))

(define p1 (<and> (<w+>)
                  (eof)))
(test (list "bccjabc" "") (parse-string "bccjabc" p1))

(define p2 (<and> (<s*>)))
(test (list " \n\t     ") (parse-string " \n\t     " p2))

(test-end "regexp")
