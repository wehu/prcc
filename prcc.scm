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

(module prcc (parse
              char
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
              act)
  
  (import chicken scheme)

  (use srfi-1)
  (use srfi-14)
  (use srfi-41)
  (use srfi-69)
  (use streams-utils)

  (define pos 0)
  (define input-stream)

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
        (hash-table-set! cache comb-id (p)))
      (hash-table-ref cache comb-id)))

  (define (char c)
    (lambda ()
      (let ((ic (stream-ref input-stream pos)))
        (if (equal? ic c)
          (begin
            (set! pos (+ pos 1))
            (char-set->string (char-set c)))
          #f))))

  (define (seq fp . lst)
    (lambda ()
      (let ((cpos pos)
            (lr (fold (lambda (cp pr)
                    (if pr
                      (let ((cr (apply-c cp)))
                        (if cr
                          (append pr (list cr))
                          #f))
                      #f))
                (let ((fr (apply-c fp)))
                  (if fr
                    (list fr)
                    #f))
                lst)))
         (if lr
           lr
           (begin
             (set! pos cpos)
             #f)))))

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

  (define (rep p)
    (lambda ()
      (letrec ((lp (lambda (r)
                  (let ((rr (apply-c p)))
                    (if rr
                      (lp (append r (list rr)))
                      r)))))
        (lp `()))))

  (define (zero)
    (lambda ()
      ""))

  (define (one? p)
    (sel p zero))

  (define (rep+ p)
    (seq p (rep p)))

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

  (define (eof)
    (lambda ()
      (if (= (+ pos 1) (stream-length input-stream))
        ""
        #f)))

  (define (str s)
    (apply seq
      (map
        (lambda (c)
          (char c))
        (char-set->list (string->char-set s)))))

  (define (one-of str)
    (apply sel
      (map
        (lambda (c)
          (char c))
        (char-set->list (string->char-set str)))))

  (define (act p proc)
    (lambda ()
      (let ((pr (p)))
        (if pr
          (proc pr)
          #f))))

  (define (parse file p)
    (set! input-stream (file->stream file))
    (p)))


