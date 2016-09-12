(use prcc)

(use test)

(test-begin "str")

(define p (sel (str "aaa") (str "")))

(test-assert (parse-string "" p))
(test-assert (parse-string "aaa" p))

(test-end "str")
