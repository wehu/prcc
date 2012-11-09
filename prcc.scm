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
              seq
              sel
              one?
              rep
              rep+
              pred
              pred!
              eof
              str
              one-of
              join
              act
              ind
	      lazy
	      regexp
              parse-file
              parse-string
              parse-port)
  
  (import chicken scheme)

  (use srfi-1)
  (use srfi-14)
  (use srfi-41)
  (use srfi-69)
  (use streams-utils)

  (define-record ctxt
    input-stream
    pos
    line
    col
    err-line
    err-col
    err-msg
    cache
    combinator-ids)

  ;; initialize context
  (define (init-ctxt ctxt)
    (ctxt-pos-set!  ctxt  0)
    (ctxt-line-set! ctxt 0)
    (ctxt-col-set!  ctxt 0)
    (ctxt-err-line-set! ctxt 0)
    (ctxt-err-col-set! ctxt 0)
    (ctxt-err-msg-set! ctxt "")
    (ctxt-cache-set! ctxt (make-hash-table))
    (ctxt-combinator-ids-set! ctxt (make-hash-table)))

  (define (%make-ctxt s)
    (let ((ctxt (make-ctxt
                  s
                  0
                  0
                  0
                  0
                  0
                  ""
                  (make-hash-table)
                  (make-hash-table))))
      ctxt))

  ;; cache computation results based on combintor and stream position
  (define (combinator-id p ctxt)
    (let ((cids (ctxt-combinator-ids ctxt)))
      (if (not (hash-table-exists? cids p))
        (hash-table-set! cids p (symbol->string (gensym))))
      (hash-table-ref cids p)))

  (define (apply-c p ctxt)
    (let* ((p-id (combinator-id p ctxt))
           (comb-id (string-append p-id ":" (number->string (ctxt-pos ctxt))))
           (cache (ctxt-cache ctxt)))
      (if (not (hash-table-exists? cache comb-id))
        (hash-table-set! cache comb-id
          (list (p ctxt)
                (ctxt-pos ctxt)
                (ctxt-line ctxt)
                (ctxt-col ctxt)
                (ctxt-err-line ctxt)
                (ctxt-err-col ctxt)
                (ctxt-err-msg ctxt))))
      (let* ((r (hash-table-ref cache comb-id))
             (rr (list-ref r 0)))
        (ctxt-pos-set! ctxt   (list-ref r 1))
        (ctxt-line-set! ctxt  (list-ref r 2))
        (ctxt-col-set! ctxt   (list-ref r 3))
        (if (not r)
          (begin
            (ctxt-err-line-set! ctxt (list-ref r 4))
            (ctxt-err-col-set! ctxt  (list-ref r 5))
            (ctxt-err-msg-set! ctxt  (list-ref r 6))))
        rr)))

  ;; end of stream
  (define (end-of-stream? i ctxt)
    (let ((es (stream-drop i (ctxt-input-stream ctxt))))
      (stream-null? es)))

  ;; error report
  (define (report-error ctxt)
    (display "parsing failed:\n")
    (display (ctxt-err-msg ctxt))
    (display "@(")
    (display (ctxt-err-line ctxt))
    (display ", ")
    (display (ctxt-err-col ctxt))
    (display ")\n"))

  (define (record-error ctxt . msg)
    (ctxt-err-line-set! ctxt (ctxt-line ctxt))
    (ctxt-err-col-set!  ctxt (ctxt-col ctxt))
    (ctxt-err-msg-set!  ctxt msg))

  ;; parse a char
  (define (char c)
    (lambda (ctxt)
      (if (end-of-stream? (ctxt-pos ctxt) ctxt)
        (begin
          (record-error ctxt "end of stream")
          #f)
        (let ((ic (stream-ref (ctxt-input-stream ctxt) (ctxt-pos ctxt))))
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

  ;; seqence of parsers
  (define (seq fp . lst)
    (lambda (ctxt)
      (let* ((cpos (ctxt-pos ctxt))
             (fr (apply-c fp ctxt))
             (lr (fold (lambda (cp pr)
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

  ;; ordered selective parsers
  (define (sel . lst)
    (lambda (ctxt)
      (fold (lambda (cp r) 
              (if r
                r
                (let ((cr (apply-c cp ctxt)))
                  (if cr
                    cr
                    #f))))
        #f
        lst)))

  ;; repeat 0 - infinite times
  (define (rep p)
    (lambda (ctxt)
      (letrec ((lp (lambda (r)
                  (let ((rr (apply-c p ctxt)))
                    (if rr
                      (lp (append r (list rr)))
                      r)))))
        (lp `()))))

  ;; null
  (define (zero)
    (lambda (ctxt)
      ""))

  ;; appear once or zero
  (define (one? p)
    (sel p zero))

  ;; repeat 1 - infinite times
  (define (rep+ p)
    (act
      (seq p (rep p))
      (lambda (o)
        (cons (car o) (cadr o)))))

  ;; predicate
  (define (pred p pd #!optional (n #f))
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

  (define (pred! p pd)
    (pred p pd #t))

  ;; end of file
  (define (eof)
    (lambda (ctxt)
      (if (end-of-stream? (ctxt-pos ctxt) ctxt)
        ""
        (begin
          (record-error ctxt "expect: end of file")
          #f))))

  ;; a string
  (define (str s)
    (act
      (apply seq
        (map
          (lambda (c)
            (char c))
          (string->list s)))
      (lambda (cs)
        (apply string-append cs))))

  ;; match one char in a string
  (define (one-of str)
    (apply sel
      (map
        (lambda (c)
          (char c))
        (string->list str))))

  ;; add action for parser to process the output
  (define (act p #!optional (succ #f) (fail #f))
    (lambda (ctxt)
      (let ((pr (p ctxt)))
        (if pr
	  (if succ
            (succ pr)
	    pr)
          (begin
	    (if fail
  	      (fail))
	    #f)))))

  ;; join 
  (define (join p0 p1)
    (act
      (seq p0 (act
                (rep (seq p1 p0))
                (lambda (o)
                  (apply append o))))
      (lambda (o)
        (cons (car o) (cadr o)))))

  ;; index
  (define (ind p index)
    (act
      p
      (lambda (o)
        (list-ref o index))))

  ;; lazy
  (define (%lazy p)
    (lambda (ctxt)
      (p ctxt)))

  (define-syntax lazy
    (syntax-rules ()
      ((_ p)
       (%lazy (lambda (ctxt)
		(p ctxt))))))

  ;; regexp
  (define (regexp)
    (letrec ((w (one-of "abcdefghijklmnopqrstuvwxyz_"))
	     (d (one-of "0123456789"))
	     (dot (char #\.))
	     (^ (char #\^))
	     ($ (char #\$))
	     (a  (sel (lazy es) (lazy ee) w d dot ^ $))
	     (o? (seq a (char #\?)))
	     (r* (seq a (char #\*)))
	     (r+ (seq a (char #\+)))
	     (es (seq (char #\[) (rep+ a) (char #\])))
	     (s  (seq (lazy e) (char #\|) (lazy e)))
             (ss (seq (char #\() s (char #\))))
	     (ee (seq (char #\() (lazy e) (char #\))))
	     (e  (seq (rep (sel o? ss r* r+ a)))))
      (seq e (eof))))

  ;; parse
  (define (parse p s)
    (let* ((ctxt (%make-ctxt s))
           (r (p ctxt)))
      (if r r
        (begin
          (report-error ctxt)
          #f))))

  ;; parse file
  (define (parse-file file p)
    (parse p (file->stream file)))

  ;; parse string
  (define (parse-string str p)
    (parse p (list->stream (string->list str))))
  
  ;; parse from port
  (define (parse-port port p)
    (parse p (port->stream port))))


