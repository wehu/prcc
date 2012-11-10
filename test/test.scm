(use prcc)

(include "act.scm")
(include "eof.scm")
(include "ind.scm")
(include "join.scm")
(include "lazy.scm")
(include "neg.scm")
(include "one_of.scm")
(include "one.scm")
(include "pred_neg.scm")
(include "pred.scm")
(include "repp.scm")
(include "rep.scm")
(include "sel.scm")
(include "seq.scm")
(include "regexp.scm")

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

(define p (<or> (char #\a) (char #\b)))
(display (parse-string "bcbbab44444abababaccc" p))
(newline)

