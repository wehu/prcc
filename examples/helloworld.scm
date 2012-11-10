(use prcc)

(define parser
  (<and>
    (<@> (<s> "hello")
      (lambda (o) "hello "))
    (<s> "world")
    (eof)))

(display (parse-string "helloworld" parser))
(newline)

