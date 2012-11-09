(use prcc)

(define parser (seq
                 (act (str "abc")
                   (lambda (o) "a"))
                 (str "bb")
                 (ind (seq
                        (str "a")
                        (str "b"))
                   1)
                 (rep+ (char #\4))
                 (join (char #\a) (char #\b))
                 (str "ccc")
                 (eof)))

(display (parse-string "abcbbab44444abababaccc" parser))
(newline)
