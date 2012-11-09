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

  (define pos  0)
  (define line 0)
  (define col  0)
  (define input-stream)

  ;; cache computation results based on combintor and stream position
  (define cache (make-hash-table))
  (define combinator-ids (make-hash-table))

  (define (combinator-id p)
    (if (not (hash-table-exists? combinator-ids p))
      (hash-table-set! combinator-ids p (symbol->string (gensym))))
    (hash-table-ref combinator-ids p))

  (define (apply-c p)
    (let* ((p-id (combinator-id p))
           (comb-id (string-append p-id ":" (number->string pos))))
      (if (not (hash-table-exists? cache comb-id))
        (hash-table-set! cache comb-id (list (p) pos line col err-line err-col err-msg)))
      (let ((r (hash-table-ref cache comb-id)))
        (set! pos      (list-ref r 1))
        (set! line     (list-ref r 2))
        (set! col      (list-ref r 3))
        (set! err-line (list-ref r 4))
        (set! err-col  (list-ref r 5))
        (set! err-msg  (list-ref r 6))
        (list-ref r 0))))

  ;; end of stream
  (define (end-of-stream? i)
    (let ((es (stream-drop i input-stream)))
      (stream-null? es)))

  ;; error report
  (define err-line 0)
  (define err-col  0)
  (define err-msg  "")

  (define (report-error)
    (display "parsing failed:\n")
    (display err-msg)
    (display "@(")
    (display err-line)
    (display ", ")
    (display err-col)
    (display ")\n"))

  (define (record-error . msg)
    (set! err-line line)
    (set! err-col  col)
    (set! err-msg  msg))

  ;; parse a char
  (define (char c)
    (lambda ()
      (if (end-of-stream? pos)
        (begin
          (record-error "end of stream")
          #f)
        (let ((ic (stream-ref input-stream pos)))
          (if (equal? ic c)
             (begin
               (set! pos (+ pos 1))
               (set! col (+ col 1))
               (if (equal? ic #\newline)
                 (begin
                   (set! col 0)
                   (set! line (+ line 1))))
               (char-set->string (char-set c)))
             (begin
               (record-error "expect:" c ";but got:" ic)
               #f))))))

  ;; seqence of parsers
  (define (seq fp . lst)
    (lambda ()
      (let* ((cpos pos)
             (fr (apply-c fp))
             (lr (fold (lambda (cp pr)
                   (if pr
                     (let ((cr (apply-c cp)))
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
             (set! pos cpos)
             #f)))))

  ;; ordered selective parsers
  (define (sel . lst)
    (lambda ()
      (fold (lambda (cp r) 
              (if r
                r
                (let ((cr (apply-c cp)))
                  (if cr
                    cr
                    #f))))
        #f
        lst)))

  ;; repeat 0 - infinite times
  (define (rep p)
    (lambda ()
      (letrec ((lp (lambda (r)
                  (let ((rr (apply-c p)))
                    (if rr
                      (lp (append r (list rr)))
                      r)))))
        (lp `()))))

  ;; null
  (define (zero)
    (lambda ()
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
    (lambda ()
      (let ((cpos pos) 
            (pr (apply-c p)))
        (if pr
          (let ((cppos pos)
                (pdr (apply-c pd)))
            (if (if n (not pdr) pdr)
              (begin
                (set! pos cppos)
                pr)
              (begin
                (set! pos cpos)
                #f)))
          #f))))

  (define (pred! p pd)
    (pred p pd #t))

  ;; end of file
  (define (eof)
    (lambda ()
      (if (end-of-stream? pos)
        ""
        (begin
          (record-error "expect: end of file")
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
    (lambda ()
      (let ((pr (p)))
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
    (lambda ()
      (p)))

  (define-syntax lazy
    (syntax-rules ()
      ((_ p)
       (%lazy (lambda ()
		(p))))))

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
	     (s (seq (lazy e) (char #\|) (lazy e)))
	     (ee (seq (char #\() (lazy e) (char #\))))
	     (e (seq (rep (sel o? a)) (eof))))
      e))

  ;; initialize parser
  (define (init-parser)
    (set! pos  0)
    (set! line 0)
    (set! col  0)
    (set! err-line 0)
    (set! err-col  0)
    (set! err-msg  ""))

  ;; parse
  (define (parse p)
    (init-parser)
    (let ((r (p)))
      (if r r
        (begin
          (report-error)
          #f))))

  ;; parse file
  (define (parse-file file p)
    (set! input-stream (file->stream file))
    (parse p))

  ;; parse string
  (define (parse-string str p)
    (set! input-stream (list->stream (string->list str)))
    (parse p))
  
  ;; parse from port
  (define (parse-port port p)
    (set! input-stream (port->stream port))
    (parse p)))


