(use prcc)

(define parser (seq
                 (act (str "abc")
                   (lambda (o) "a"))
                 (str "bb")
                 (rep+ (char #\4))
                 (join (char #\a) (char #\b))
                 (eof)))

(display (parse-string "abcbb44444abababa" parser))
(newline)
