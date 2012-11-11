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
  (use type-checks)
  (use regex)
  (use data-structures)
  (use utils)

  (define-record ctxt
    name
    input-stream
    input-stream-length
    pos
    line
    col
    err-pos
    err-line
    err-col
    err-msg
    cache)

  ;; initialize context
  (define (init-ctxt ctxt)
    (ctxt-pos-set!  ctxt  0)
    (ctxt-line-set! ctxt 0)
    (ctxt-col-set!  ctxt 0)
    (ctxt-err-pos-set! ctxt 0)
    (ctxt-err-line-set! ctxt 0)
    (ctxt-err-col-set! ctxt 0)
    (ctxt-err-msg-set! ctxt "")
    (ctxt-cache-set! ctxt (make-hash-table)))

  (define (%make-ctxt n s)
    (let ((ctxt (make-ctxt
                  n
                  (list->vector (string->list s))
                  (string-length s)
                  0
                  0
                  0
                  0
                  0
                  0
                  ""
                  (make-hash-table))))
      ctxt))

  ;; cache computation results based on combintor and stream position
  (define (combinator-cache p ctxt)
    (let ((c (ctxt-cache ctxt)))
      (if (not (hash-table-exists? c p))
        (hash-table-set! c p (make-hash-table)))
      (hash-table-ref c p)))

  (define (apply-c p ctxt)
    (let* ((cache (combinator-cache p ctxt))
           (id (ctxt-pos ctxt)))
      (if (not (hash-table-exists? cache id))
        (hash-table-set! cache id
          (list (p ctxt)
                (ctxt-pos ctxt)
                (ctxt-line ctxt)
                (ctxt-col ctxt)
                (ctxt-err-pos ctxt)
                (ctxt-err-line ctxt)
                (ctxt-err-col ctxt)
                (ctxt-err-msg ctxt))))
      (let* ((r (hash-table-ref cache id))
             (rr (list-ref r 0)))
        (ctxt-pos-set! ctxt   (list-ref r 1))
        (ctxt-line-set! ctxt  (list-ref r 2))
        (ctxt-col-set! ctxt   (list-ref r 3))
        (if (not r)
          (begin
            (ctxt-err-pos-set! ctxt (list-ref r 4))
            (ctxt-err-line-set! ctxt (list-ref r 5))
            (ctxt-err-col-set! ctxt  (list-ref r 6))
            (ctxt-err-msg-set! ctxt  (list-ref r 7))))
        rr)))

  ;; end of stream
  (define (end-of-stream? i ctxt)
    (>= (+ i 1) (ctxt-input-stream-length ctxt)))

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

  ;; parse a char
  (define (char c)
    (check-char 'char c)
    (lambda (ctxt)
      (if (end-of-stream? (ctxt-pos ctxt) ctxt)
        (begin
          (record-error ctxt "end of stream")
          #f)
        (let ((ic (vector-ref (ctxt-input-stream ctxt) (ctxt-pos ctxt))))
          (if (equal? ic c)
             (begin
               (ctxt-pos-set! ctxt (+ (ctxt-pos ctxt) 1))
               (ctxt-col-set! ctxt (+ (ctxt-col ctxt) 1))
               (if (equal? ic #\newline)
                 (begin
                   (ctxt-col-set! ctxt 0)
                   (ctxt-line-set! ctxt (+ (ctxt-line ctxt) 1))))
               (char-set->string (char-set c)))
             (begin
               (record-error ctxt "expect:" c ";but got:" ic)
               #f))))))
  (define <c> char)

  ;; seqence of parsers
  (define (seq fp . lst)
    (check-procedure 'seq fp)
    (lambda (ctxt)
      (let* ((cpos (ctxt-pos ctxt))
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
             (ctxt-pos-set! ctxt cpos)
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
      (let ((cpos (ctxt-pos ctxt))
            (pr (apply-c p ctxt)))
        (if pr
          (let ((cppos (ctxt-pos ctxt))
                (pdr (apply-c pd ctxt)))
            (if (if n (not pdr) pdr)
              (begin
                (ctxt-pos-set! ctxt cppos)
                pr)
              (begin
                (ctxt-pos-set! ctxt cpos)
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
      (if (end-of-stream? (ctxt-pos ctxt) ctxt)
        ""
        (begin
          (record-error ctxt "expect: end of file")
          #f))))

  ;; neg
  (define (neg p)
    (check-procedure 'neg p)
    (lambda (ctxt)
      (let ((cpos (ctxt-pos ctxt))
            (r (apply-c p ctxt)))
        (if r
          (begin
            (ctxt-pos-set! ctxt cpos)
            (record-error ctxt "expect: parsing failure")
            #f)
          (begin
            (ctxt-pos-set! ctxt (+ (ctxt-err-pos ctxt) 1))
            (let ((s (subvector (ctxt-input-stream ctxt) cpos (ctxt-pos ctxt))))
              (list->string (vector->list s))))))))
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
                (fail (if (end-of-stream? (ctxt-err-pos ctxt) ctxt)
                        ""
                        (vector-ref (ctxt-input-stream ctxt) (ctxt-err-pos ctxt))))))
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

  (define (update-line-col str ctxt)
    (let* ((sr (string-split str "\n" #t))
           (sll (string-length (car (reverse sr)))))
      (if (= (length sr) 1)
        (ctxt-col-set! ctxt (+ (ctxt-col ctxt) sll))
        (ctxt-col-set! ctxt sll))
        (ctxt-line-set! ctxt (+ (ctxt-line ctxt) (- (length sr) 1)))))

  (define (regexp-parser r)
    (check-string 'regexp-parser r)
    (lambda (ctxt)
      (if (not (end-of-stream? (ctxt-pos ctxt) ctxt))
        (let ((str (list->string (vector->list
                     (subvector (ctxt-input-stream ctxt) (ctxt-pos ctxt)
                       (- (ctxt-input-stream-length ctxt) 1))))))
          (let ((rr (string-search (regexp (string-append "^" r)) str)))
            (if rr
              (let ((rrr (car rr)))
                (ctxt-pos-set! ctxt (+ (ctxt-pos ctxt) (string-length rrr)))
                (update-line-col rrr ctxt)
                rrr)
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
    (let ((l (string-length s)))
      (lambda (ctxt)
        (let ((npos (+ (ctxt-pos ctxt) l)))
          (if (not (end-of-stream? (- npos 1) ctxt))
            (let ((ss (list->string (vector->list
                        (subvector (ctxt-input-stream ctxt)
                                 (ctxt-pos ctxt)
                                 npos)))))
              (if (equal? s ss)
                (begin
                  (ctxt-pos-set! ctxt npos)
                  (update-line-col ss ctxt)
                  s)
                (begin
                  (record-error ctxt "expect:" s ";but got:" ss)
                  #f)))
            (begin
              (record-error ctxt "expect:" s ";but got: end of file")
              #f))))))
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
  (define (parse p n s)
    (let* ((ctxt (%make-ctxt n (string-append s "0")))
           (r (p ctxt)))
      (if r r
        (begin
          (report-error ctxt)
          #f))))

  ;; parse file
  (define (parse-file file p)
    (check-string 'parse-file file)
    (check-procedure 'parse-file p)
    (parse p file (read-all file)))

  ;; parse string
  (define (parse-string str p)
    (check-string 'parse-string str)
    (check-procedure 'parse-string p)
    (parse p str str))
  
  ;; parse from port
  (define (parse-port port p)
    (check-input-port 'parse-port port)
    (check-procedure 'parse-port p)
    (parse p (port-name) (read-all port))))


