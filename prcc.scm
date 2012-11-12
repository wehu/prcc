;;;
;; Copyright 2012 The Prcc Authors. All Rights Reserved.
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;;;

(module prcc (char
              <c>
              seq
              <and>
              sel
              <or>
              one?
              <?>
              rep
              <*>
	      rep_
              <*_>
              rep+
              <+>
	      rep+_
              <+_>
              pred
              <&>
              pred!
              <&!>
              eof
              act
              <@>
	      lazy
              neg
              <^>
              regexp-parser
              <r>
              ;; helpers
              str
              <s>
              one-of
              join+
              join+_
              ind
              <#>
              <w>
              <space>
              <w*>
              <s*>
              <w+>
              <s+>
              even
              odd
              seq_
              <and_>
              ;;
              parse-file
              parse-string
              parse-port)
  
  (import chicken scheme)

  (use srfi-1)
  (use srfi-14)
  (use srfi-69)
  (use srfi-13)
  (use stack)
  (use type-checks)
  (use regex)
  (use data-structures)
  (use utils)

  (define-record ctxt
    name
    input-stream
    stack
    input-stream-length
    input-cache
    pos
    line
    col
    err-pos
    err-line
    err-col
    err-msg
    caching?
    cache)

  (define (%make-ctxt n s #!optional (c? #f))
    (let ((ctxt (make-ctxt
                  n
                  s
                  (make-stack)
                  (string-length s)
                  (make-hash-table)
                  0
                  0
                  0
                  0
                  0
                  0
                  ""
                  c?
                  (make-hash-table))))
      ctxt))

  ;; cache computation results based on combintor and stream position
  (define (combinator-cache p ctxt)
    (let ((c (ctxt-cache ctxt)))
      (if (not (hash-table-exists? c p))
        (hash-table-set! c p (make-hash-table)))
      (hash-table-ref c p)))

  (define (stack-copy s)
    (let ((ns (make-stack)))
      (for-each (lambda (e)
          (stack-push! ns e))
        (reverse (stack->list s)))
      ns))

  (define (apply-c p ctxt)
    (if (ctxt-caching? ctxt)
      (let* ((cache (combinator-cache p ctxt))
             (id (ctxt-pos ctxt)))
        (if (hash-table-exists? cache id)
          (let ((r (hash-table-ref cache id)))
            (read-chars (- (list-ref r 1) id) ctxt))
          (let ((r (p ctxt)))
            (hash-table-set! cache id
              (list r
                    (ctxt-pos ctxt)
                    (ctxt-err-pos ctxt)
                    (ctxt-err-line ctxt)
                    (ctxt-err-col ctxt)
                    (ctxt-err-msg ctxt)))))
        (let* ((r (hash-table-ref cache id))
               (rr (list-ref r 0)))
          (if (not rr)
            (begin
              (ctxt-err-pos-set! ctxt (list-ref r 2))
              (ctxt-err-line-set! ctxt (list-ref r 3))
              (ctxt-err-col-set! ctxt  (list-ref r 4))
              (ctxt-err-msg-set! ctxt  (list-ref r 5))))
          rr))
      (p ctxt)))

  ;; end of stream
  (define (end-of-stream? ctxt)
    (string-null? (ctxt-input-stream ctxt)))

  ;; error report
  (define (report-error ctxt)
    (display "parsing \'")
    (display (ctxt-name ctxt))
    (display "\' failed:\n\t")
    (display (ctxt-err-msg ctxt))
    (display "@(")
    (display (+ (ctxt-err-line ctxt) 1))
    (display ", ")
    (display (+ (ctxt-err-col ctxt) 1))
    (display ")\n"))

  (define (record-error ctxt . msg)
    (ctxt-err-pos-set!  ctxt (ctxt-pos ctxt))
    (ctxt-err-line-set! ctxt (ctxt-line ctxt))
    (ctxt-err-col-set!  ctxt (ctxt-col ctxt))
    (ctxt-err-msg-set!  ctxt msg))

  ;; read/rewind pair for performance
  (define (update-pos-line-col str ctxt #!optional (op +))
    (ctxt-pos-set! ctxt (op (ctxt-pos ctxt) (string-length str)))
    (let* ((ss (string-split str "\n" #t))
           (ssl (length ss))
           (ssll (string-length (car (reverse ss)))))
      (if (= ssl 1)
        (ctxt-col-set! ctxt (op (ctxt-col ctxt) ssll))
        (ctxt-col-set! ctxt ssll))
      (ctxt-line-set! ctxt (op (ctxt-line ctxt) (- ssl 1)))))

  ;; cache string operation?
  (define (substring-c n ctxt)
    (let* ((sl (ctxt-input-stream-length ctxt))
           (id (cons (+ (ctxt-pos ctxt) n) sl))
           (cache (ctxt-input-cache ctxt)))
      (if (not (hash-table-exists? cache id))
        (hash-table-set! cache id (substring/shared (ctxt-input-stream ctxt) n)))
      (hash-table-ref cache id)))

  (define (string-take-c n ctxt)
    (substring-c 0 ctxt)
    (let* ((id (cons (ctxt-pos ctxt) (+ (ctxt-pos ctxt) n)))
           (cache (ctxt-input-cache ctxt)))
      (if (not (hash-table-exists? cache id))
        (hash-table-set! cache id (string-take (ctxt-input-stream ctxt) n)))
      (ctxt-input-stream-set! ctxt (substring-c n ctxt))
      (hash-table-ref cache id)))

  (define (string-rewind-c s ctxt)
    (let* ((sl (ctxt-input-stream-length ctxt))
           (id (cons (- (ctxt-pos ctxt) (string-length s)) sl))
           (cache (ctxt-input-cache ctxt)))
      (if (not (hash-table-exists? cache id))
        (hash-table-set! cache id (string-append/shared s (ctxt-input-stream ctxt))))
      (ctxt-input-stream-set! ctxt (hash-table-ref cache id))))

  (define (read-chars n ctxt)
    (let ((str (string-take-c n ctxt)))
      (stack-push! (ctxt-stack ctxt) str)
      (update-pos-line-col str ctxt)
      str))

  (define (rewind n ctxt)
    (letrec ((l (lambda (i)
        (if (not (= i n))
          (let ((str (stack-pop! (ctxt-stack ctxt))))
            (string-rewind-c str ctxt)
            (update-pos-line-col str ctxt -)
            (l (+ i 1)))))))
      (l 0)))

  ;; parse a char
  (define (char c)
    (check-char 'char c)
    (lambda (ctxt)
      (if (end-of-stream? ctxt)
        (begin
          (record-error ctxt "end of stream")
          #f)
        (let ((ic (string-ref (ctxt-input-stream ctxt) 0)))
          (if (equal? ic c)
             (read-chars 1 ctxt) 
             (begin
               (record-error ctxt "expect:" c ";but got:" ic)
               #f))))))
  (define <c> char)

  ;; seqence of parsers
  (define (seq fp . lst)
    (check-procedure 'seq fp)
    (lambda (ctxt)
      (let* ((csc (stack-count (ctxt-stack ctxt)))
             (fr (apply-c fp ctxt))
             (lr (fold (lambda (cp pr)
                   (check-procedure 'seq cp)
                   (if pr
                     (let ((cr (apply-c cp ctxt)))
                       (if cr
                         (append pr (list cr))
                         #f))
                     #f))
                   (if fr
                     (list fr)
                     #f)
                   lst)))
         (if lr
           lr
           (begin
             (rewind (- (stack-count (ctxt-stack ctxt)) csc) ctxt)
             #f)))))
  (define <and> seq)

  ;; ordered selective parsers
  (define (sel . lst)
    (lambda (ctxt)
      (fold (lambda (cp r)
              (check-procedure 'sel cp) 
              (if r
                r
                (let ((cr (apply-c cp ctxt)))
                  (if cr
                    cr
                    #f))))
        #f
        lst)))
  (define <or> sel)

  ;; repeat 0 - infinite times
  (define (rep p)
    (check-procedure 'rep p)
    (lambda (ctxt)
      (letrec ((lp (lambda (r)
                  (let ((rr (apply-c p ctxt)))
                    (if rr
                      (lp (append r (list rr)))
                      r)))))
        (lp `()))))
  (define <*> rep)

  ;; null
  (define (zero)
    (lambda (ctxt)
      ""))
  (define <null> zero)

  ;; appear once or zero
  (define (one? p)
    (check-procedure 'one? p)
    (sel p (zero)))
  (define <?> one?)

  ;; repeat 1 - infinite times
  (define (rep+ p)
    (check-procedure 'rep+ p)
    (act
      (seq p (rep p))
      (lambda (o)
        (cons (car o) (cadr o)))))
  (define <+> rep+)

  ;; predicate
  (define (pred p pd #!optional (n #f))
    (check-procedure 'pred p)
    (check-procedure 'pred pd)
    (lambda (ctxt)
      (let ((pr (apply-c p ctxt)))
        (if pr
          (let ((pdr (apply-c pd ctxt)))
            (if (if n (not pdr) pdr)
              (begin
                (if (not n) (rewind 1 ctxt))
                pr)
              (begin
                (if n (rewind 1 ctxt))
                #f)))
          #f))))
  (define <&> pred)

  (define (pred! p pd)
    (check-procedure 'pred! p)
    (check-procedure 'pred! pd)
    (pred p pd #t))
  (define <&!> pred!)

  ;; end of file
  (define (eof)
    (lambda (ctxt)
      (if (end-of-stream? ctxt)
        ""
        (begin
          (record-error ctxt "expect: end of file")
          #f))))

  ;; neg
  (define (neg p)
    (check-procedure 'neg p)
    (lambda (ctxt)
      (let ((csc (stack-count (ctxt-stack ctxt)))
            (r (apply-c p ctxt)))
        (if r
          (begin
            (rewind (- (stack-count (ctxt-stack ctxt)) csc) ctxt)
            (record-error ctxt "expect: parsing failure")
            #f)
            (read-chars (- (+ (ctxt-err-pos ctxt) 1) (ctxt-pos ctxt)) ctxt)))))
  (define <^> neg)

  ;; add action for parser to process the output
  (define (act p #!optional (succ #f) (fail #f))
    (check-procedure 'act p)
    (lambda (ctxt)
      (let ((pr (apply-c p ctxt)))
        (if pr
	  (if succ
            (begin
              (check-procedure 'act succ)
              (succ pr))
	    pr)
          (begin
	    (if fail
  	      (begin
                (check-procedure 'act fail)
                (fail (ctxt-err-msg ctxt))))
	    #f)))))
  (define <@> act)

  ;; lazy
  (define-syntax lazy
    (syntax-rules ()
      ((_ p)
       (lambda (ctxt)
         ((lambda (c)
            (check-procedure 'lazy p)
            (p c)) ctxt)))))

  ;; regexp
  (define (regexp-parser r)
    (check-string 'regexp-parser r)
    (lambda (ctxt)
      (if (not (end-of-stream? ctxt))
        (let ((str (ctxt-input-stream ctxt))
              (re (regexp (string-append "^" r))))
          (let ((rr (string-search re str)))
            (if rr
              (let ((rrr (car rr)))
                (read-chars (string-length rrr) ctxt))
              (begin
                (record-error ctxt "regexp \'" r "\' match failed")
                #f))))
         (begin
           (record-error ctxt "expect:" r ";but got: end of file")
           #f))))
  (define <r> regexp-parser)

  ;; helpers

  ;; a string
  (define (str s)
    (check-string 'str s)
    (act
      (apply seq
        (map (lambda (c)
          (char c))
          (string->list s)))
      (lambda (o)
        (apply string-append o))))
  (define <s> str)

  ;; match one char in a string
  (define (one-of str)
    (check-string 'one-of str)
    (apply sel
      (map
        (lambda (c)
          (char c))
        (string->list str))))

  ;; join
  (define (join+ p0 p1)
    (check-procedure 'join+ p0)
    (check-procedure 'join+ p1)
    (act
      (seq p0 (act
                (rep (seq p1 p0))
                (lambda (o)
                  (apply append o))))
      (lambda (o)
        (cons (car o) (cadr o)))))

  ;; index
  (define (ind p index)
    (check-procedure 'ind p)
    (check-number 'ind index)
    (act
      p
      (lambda (o)
        (list-ref o index))))
  (define <#> ind)

  (define (<w>)
    (<r> "\\w"))

  (define (<space>)
    (<r> "\\s"))

  (define (<w*>)
    (<r> "\\w*"))

  (define (<s*>)
    (<r> "\\s*"))

  (define (<w+>)
    (<r> "\\w+"))

  (define (<s+>)
    (<r> "\\s+"))

  (define (even p)
    (check-procedure 'even p)
    (act p
      (lambda (o)
        (car (fold (lambda (oo i)
          (if (even? (cdr i))
            (cons (append (car i) (list oo)) (+ (cdr i) 1))
            (cons (car i) (+ (cdr i) 1))))
          (cons `() 0)
          o)))))

  (define (odd p)
    (check-procedure 'odd p)
    (act p
      (lambda (o)
        (car (fold (lambda (oo i)
          (if (odd? (cdr i))
            (cons (append (car i) (list oo)) (+ (cdr i) 1))
            (cons (car i) (+ (cdr i) 1))))
          (cons `() 0)
          o)))))

  (define (join+_ p0 p1 #!key (skip (<s*>)))
    (check-procedure 'join+_ p0)
    (check-procedure 'join+_ p1)
    (check-procedure 'join+_ skip)
    (act (join+ p0 (seq skip p1 skip))
      (lambda (o)
        (car (fold (lambda (oo i)
            (if (even? (cdr i))
              (cons (append (car i) (list oo)) (+ (cdr i) 1))
              (cons (append (car i) (list (cadr oo))) (+ (cdr i) 1))))
          (cons `() 0)
          o)))))

  (define (seq_ #!rest lst #!key (skip (<s*>)))
    (check-procedure 'seq_ skip)
    (let* ((nlst (car (fold (lambda (p i)
                     (if (cdr i)
                       (if (equal? p #:skip)
                         (cons (car i) #f)
                         (begin
			   (check-procedure 'seq_ p)
			   (cons (append (car i) (list p)) #t)))
                       (cons (car i) #t)))
                   (cons `() #t)
                   lst)))
           (l (fold (lambda (p i)
                  (if (equal? i `())
                    (list p)
                    (append i (list skip p))))
               `()
               nlst)))
      (even (apply seq l))))
  (define <and_> seq_)

  (define (rep+_ p #!key (skip (<s*>)))
    (check-procedure 'rep+_ p)
    (check-procedure 'rep+_ skip)
    (even (join+ p skip)))
  (define <+_> rep+_)

  (define (rep_ p #!key (skip (<s*>)))
    (check-procedure 'rep_ p)
    (check-procedure 'rep_ skip)
    (<or> (rep+_ p skip)
	  (act (zero) (lambda (o) `()))))
  (define <*_> rep_)

  ;; parse
  (define (parse p n s #!optional (c? #f))
    (let* ((ctxt (%make-ctxt n s c?))
           (r (p ctxt)))
      (if r r
        (begin
          (report-error ctxt)
          #f))))

  ;; parse file
  (define (parse-file file p #!optional (c? #f))
    (check-string 'parse-file file)
    (check-procedure 'parse-file p)
    (parse p file (read-all file) c?))

  ;; parse string
  (define (parse-string str p #!optional (c? #f))
    (check-string 'parse-string str)
    (check-procedure 'parse-string p)
    (parse p str str c?))
  
  ;; parse from port
  (define (parse-port port p #!optional (c? #f))
    (check-input-port 'parse-port port)
    (check-procedure 'parse-port p)
    (parse p (port-name) (read-all port) c?)))


