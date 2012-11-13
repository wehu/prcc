(use prcc)

(define n (<r> "(\\+|-)?\\s*\\d+(\\s*\\.\\s*\\d+(\\s*(e|E)(\\s*(\\+|-))?\\s*\\d+)?)?"))
(define s (<r> "\"(\\\\u[0-9a-fA-F]{4}|\\\\n|\\\\t|\\\\b|\\\\f|\\\\r|\\\\\\\\|\\\\\"|[^\"])*\""))

(define oo (<and_> (<c> #\{) (<?> (join+_ (<and_> s (<c> #\:) (lazy v)) (<c> #\,))) (<c> #\})))
(define a (<and_> (<c> #\[) (<?> (join+_ (lazy v) (<c> #\,))) (<c> #\])))

(define v
  (<or> oo
        a
        n
        s
        (<s> "true")
        (<s> "false")
        (<s> "null")))

(define p
  (<and_> v (eof)))

(use test)

(test-group "fjson"

  (test-begin "fjson")

  (define r #f)

  (for-each (lambda (f)
    (set! r (parse-file f p))
    (test-assert r)
    (display r)
    (newline))
    (cdr (argv)))

  (test-end "fjson"))
