(use prcc)

(define true (<s> "true"))
(define false (<s> "false"))
(define null (<s> "null"))

(define e
  (<or> (<s> "e+")
        (<s> "e-")
        (<s> "E+")
        (<s> "E-")
        (<c> #\e)
        (<c> #\E)))

(define digits
  (<r> "\\d+"))

(define expp
  (<and_> e digits))

(define frac
  (<and_> (<c> #\.) digits))

(define int
  (<or> (<and_> (<c> #\-) digits)
        digits))

(define number
  (<or> (<and_> int frac expp)
        (<and_> int frac)
        (<and_> int expp)
        (<and_> int)))

(define ch
  (<or> (<s> "\\\"")
        (<s> "\\\\")
        (<s> "\\b")
        (<s> "\\f")
        (<s> "\\n")
        (<s> "\\r")
        (<s> "\\t")
        (<s> "\\u")
        (<r> "[^\\\"]")))

(define chars
  (<+> ch))

(define str
  (<or> (<and> (<c> #\") (<c> #\"))
        (<and> (<c> #\") chars (<c> #\"))))

(define value
  (<or> (lazy array)
        (lazy object)
        str
        number
        true
        false
        null))

(define pair
  (<and_> str (<c> #\:) value))

(define members
  (join+ pair (<and> (<s*>) (char #\,) (<s*>))))

(define object
  (<or> (<and_> (<c> #\{) (<c> #\}))
        (<and_> (<c> #\{) members (<c> #\}))))

(define elements
  (join+ value (<and> (<s*>) (<c> #\,) (<s*>))))

(define array
  (<or> (<and_> (<c> #\[) (<c> #\]))
        (<and_> (<c> #\[) elements (<c> #\]))))

(define parser
  (<and> (odd (join+ (<s*>) value)) (eof)))

(for-each (lambda (f)
  (display (parse-file f parser))
  (newline))
  (cdr (argv)))
