(use prcc)

(define parser
  (seq
    (act (str "hello")
      (lambda (o) "hello "))
    (str "world")
    (eof)))

(display (parse-string "helloworld" parser))
(newline)

