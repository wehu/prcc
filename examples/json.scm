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
  (<r> "(\\+|-)?\\d+(\\.\\d+)?"))
;  (<or> (<and_> int frac expp)
;        (<and_> int frac)
;        (<and_> int expp)
;        (<and_> int)))

(define ch
  (<or> (<r> "\\\\u[0-9a-f]{4}")
        (<s> "\\\"")
        (<s> "\\\\")
        (<s> "\\b")
        (<s> "\\f")
        (<s> "\\n")
        (<s> "\\r")
        (<s> "\\t")
        (<r> "[^\\\"]")))

(define chars
  (<+> ch))

(define str
  (<r> "\"(\\\\\"|[^\"])*\""))
  ;(<or> (<and> (<c> #\") (<c> #\"))
  ;      (<and> (<c> #\") chars (<c> #\"))))

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
  (<and_> value (eof)))

(use test)

(test-group "json"

  (test-begin "json")
  
  (define result #f)
  
  (for-each (lambda (f)
    (set! result (parse-file f parser))
    (test-assert result)
  ;  (display result)
    (newline))
    (cdr (argv)))
  
  (test-end "json"))
