(use prcc)

(use test)

(test-begin "act")

(define p0 (<and> (char #\a)
                (<@> (one-of "abc")
                  (lambda (o) "ooo"))
                (eof)))
(test (list "a" "ooo" "") (parse-string "ab" p0))

(define p1 (<and> (char #\a)
                (<@> (one-of "abc")
                  (lambda (o) "ooo")
                  (lambda (o) (display "meeting \'e\'")))
                (eof)))
(test-assert (not (parse-string "ae" p1)))

(test-end "act")
