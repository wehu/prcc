(use prcc)

(use test)

(test-begin "seq_")

(define p0 (<and_> (char #\a)
                   (char #\a)))
(test (list "a" "a") (parse-string "a   abbb" p0))


(define p1 (<and_> (char #\a)
                   (char #\a)
                   skip: (<c> #\,)))
(test (list "a" "a") (parse-string "a,a" p1))


(define p2 (seq_ (str "asdf") (eof)))

(test-assert (parse-string "asdf" p2))

(test-end "seq_")
