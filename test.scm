(use prcc)

(define parser (seq
                 (act (str "abc")
                   (lambda (o) "a"))
                 (str "bb")
                 (rep+ (char #\4))
                 (eof)))

(display (parse-string "abcbb44444" parser))
(newline)
