;; Copyright (C) 2020 Wolfgang Corcoran-Mathe

;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be included
;; in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
;; OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(import (scheme base)
        (srfi 189)
        (srfi 64))

;;;; Utility

(define (identity x) x)

(define-syntax constantly
  (syntax-rules ()
    ((_ obj) (lambda _ obj))))

;; Gives the values of expr as a list.
(define-syntax values->list
  (syntax-rules ()
    ((_ expr)
     (call-with-values (lambda () expr) list))))

(define always (constantly #t))
(define never (constantly #f))

;; Verify that a Maybe is a Just of 'z, a dummy object.
(define (just-of-z? m)
  (and (maybe? m) (maybe= eqv? m (just 'z))))

;; Verify that an Either is a Right of 'z, a dummy object.
(define (right-of-z? e)
  (and (either? e) (either= eqv? e (right 'z))))

;; Verify that an Either is a Left of 'z, a dummy object.
(define (left-of-z? e)
  (and (either? e) (either= eqv? e (left 'z))))

;;;; Tests

(define (check-constructors)
  ;; Uniqueness of the Nothing object.
  (test-equal (eq? (nothing) (nothing))#t)

  ;; list->just and list->right
  (test-equal (maybe= eqv? (just #t #t) (list->just '(#t #t)))   #t)
  (test-equal (either= eqv? (right #t #t) (list->right '(#t #t)))#t)
  (test-equal (either= eqv? (left #t #t) (list->left '(#t #t)))  #t)

  ;; maybe->either and either->maybe
  (test-equal (left-of-z? (maybe->either (nothing) 'z))                   #t)
  (test-equal (right-of-z? (maybe->either (just 'z) #f))                  #t)
  (test-equal (either= eqv? (right #t #t) (maybe->either (just #t #t) #f))#t)
  (test-equal (nothing? (either->maybe (left #t)))                        #t)
  (test-equal (just-of-z? (either->maybe (right 'z)))                     #t)
  (test-equal (maybe= eqv? (just #t #t) (either->maybe (right #t #t)))    #t)

  ;; either-swap
  (test-equal (either= eqv? (right #t #t) (either-swap (left #t #t)))#t)
  (test-equal (either= eqv? (left #t #t) (either-swap (right #t #t)))#t))

;;;; Predicates

(define (check-predicates)

  (test-equal (just? (just 'z))   #t)
  (test-equal (just? (nothing))   #f)
  (test-equal (nothing? (just 'z))#f)
  (test-equal (nothing? (nothing))#t)
  (test-equal (maybe? (just 'z))  #t)
  (test-equal (maybe? (nothing))  #t)

  (test-equal (right? (right 'z)) #t)
  (test-equal (right? (left 'z))  #f)
  (test-equal (left? (right 'z))  #f)
  (test-equal (left? (left 'z))   #t)
  (test-equal (either? (right 'z))#t)
  (test-equal (either? (left 'z)) #t)

  (test-equal (maybe= eqv? (just #t) (just #t))#t)
  (test-equal (maybe= eqv? (just #t) (just #f))#f)
  (test-equal (maybe= eqv? (nothing) (nothing))#t)
  (test-equal (maybe= eqv? (just #t) (nothing))#f)

  (test-equal (maybe= eqv? (just #t #f) (just #t #f))#t)
  (test-equal (maybe= eqv? (just #t #f) (just #t 'z))#f)
  (test-equal (maybe= eqv? (just #t #f) (just #t))   #f)

  (test-equal (maybe= eqv? (just #t) (just #t) (just #t))         #t)
  (test-equal (maybe= eqv? (nothing) (nothing) (nothing))         #t)
  (test-equal (maybe= eqv? (just #t) (just #t) (nothing))         #f)
  (test-equal (maybe= eqv? (just #t) (just #t) (just #f))         #f)
  (test-equal (maybe= eqv? (just #t 'z) (just #t 'z) (just #t 'z))#t)

  (test-equal (either= eqv? (right #t) (right #t))#t)
  (test-equal (either= eqv? (right #t) (right #f))#f)
  (test-equal (either= eqv? (left #t) (left #t))  #t)
  (test-equal (either= eqv? (left #t) (left #f))  #f)
  (test-equal (either= eqv? (right #t) (left #t)) #f)

  (test-equal (either= eqv? (right #t #f) (right #t #f))#t)
  (test-equal (either= eqv? (right #t #f) (right #t 'z))#f)
  (test-equal (either= eqv? (right #t #f) (right #t))   #f)
  (test-equal (either= eqv? (left #t #f) (left #t #f))  #t)
  (test-equal (either= eqv? (left #t #f) (left #t 'z))  #f)
  (test-equal (either= eqv? (left #t #f) (left #t))     #f)
  (test-equal (either= eqv? (left #t #f) (right #t #f)) #f)

  (test-equal (either= eqv? (right #t) (right #t) (right #t))         #t)
  (test-equal (either= eqv? (left #t) (left #t) (left #t))            #t)
  (test-equal (either= eqv? (right #t) (right #t) (left #t))          #f)
  (test-equal (either= eqv? (right #t) (right #t) (right #f))         #f)
  (test-equal (either= eqv? (right #t 'z) (right #t 'z) (right #t 'z))#t))

;;;; Accessors

(define (check-accessors)
  (test-equal (maybe-ref (nothing) (lambda () #f))       #f)
  (test-equal (maybe-ref (just #t) (lambda () #f) values)#t)
  (test-equal (maybe-ref (nothing) (lambda () #f) values)#f)

  (test-equal (values->list (maybe-ref (just #t #f) (lambda () #f)))'(#t #f))
  (test-equal (maybe-ref (just #t #f) (lambda () #f) list)          '(#t #f))

  (test-equal (either-ref (left #t) (constantly #f))        #f)
  (test-equal (either-ref (right #t) (constantly #f) values)#t)
  (test-equal (either-ref (left #t) values (constantly #f)) #t)

  (test-equal (either-ref (right #t #f) (constantly #f) list)'(#t #f))
  (test-equal (either-ref (left #t #f) list (constantly #f)) '(#t #f))

  (test-equal (maybe-ref/default (just #t) #f)#t)
  (test-equal (maybe-ref/default (nothing) #f)#f)
  (test-equal (values->list (maybe-ref/default (just #t #t) #f #f))'(#t #t))
  (test-equal (values->list (maybe-ref/default (nothing) #f #f))   '(#f #f))

  (test-equal (either-ref/default (right #t) #f)#t)
  (test-equal (either-ref/default (left #t) #f) #f)
  (test-equal (values->list (either-ref/default (right #t #t) #f #f))
   '(#t #t))
  (test-equal (values->list (either-ref/default (left #t) #f #f))
   '(#f #f)))

;;;; Join and bind

(define (check-join-and-bind)
  ;; maybe-join
  (test-equal (just-of-z? (maybe-join (just (just 'z))))#t)
  (test-equal (nothing? (maybe-join (just (nothing))))  #t)
  (test-equal (nothing? (maybe-join (nothing)))         #t)

  ;; either-join
  (test-equal (right-of-z? (either-join (right (right 'z))))#t)
  (test-equal (left-of-z? (either-join (right (left 'z))))  #t)
  (test-equal (left-of-z? (either-join (left 'z)))          #t)

  ;; maybe-bind
  (test-equal (nothing? (maybe-bind (nothing) just))#t)

  (test-equal (just-of-z? (maybe-bind (just 'z) just))#t)

  (test-equal (let ((m (just #t #f)))
           (maybe= eqv? m (maybe-bind m just)))
   #t)

  ;; Associativity of bind.
  (let ((k (lambda (n) (just (* n 2))))
        (h (lambda (n) (just (+ n 5))))
        (m (just 1)))
    (test-equal (maybe= eqv?
                   (maybe-bind m (lambda (n) (maybe-bind (k n) h)))
                   (maybe-bind (maybe-bind m k) h))
     #t))

  ;; Bind with multiple mprocs.
  (let ((neg (lambda (b) (just (not b)))))
    (test-equal (maybe= eqv? (just #f) (maybe-bind (just #t) neg neg neg))
     #t)
    (test-equal (nothing? (maybe-bind (just #t) neg (constantly (nothing)) neg))
     #t))

  ;; maybe-compose
  (test-equal (nothing? ((maybe-compose (constantly (nothing))) 'z))#t)
  (test-equal (just-of-z? ((maybe-compose just) 'z))                #t)

  ;; Compose with multiple mprocs.
  (let ((neg (lambda (b) (just (not b)))))
    (test-equal (maybe= eqv? (just #t) ((maybe-compose neg neg neg) #f))
     #t))

  ;; either-bind
  (test-equal (left? (either-bind (left #f) right))#t)

  (test-equal (right-of-z? (either-bind (right 'z) right))#t)

  (test-equal (let ((e (right #t #f)))
           (either= eqv? e (either-bind e right)))
   #t)

  ;; Associativity of bind.
  (let ((k (lambda (n) (right (* n 2))))
        (h (lambda (n) (right (+ n 5))))
        (e (right 1)))
    (test-equal
     (either= eqv? (either-bind e (lambda (n) (either-bind (k n) h)))
                   (either-bind (either-bind e k) h))
    #t))

  ;; Bind with multiple mprocs.
  (let ((neg (lambda (b) (right (not b)))))
    (test-equal (either= eqv? (right #f) (either-bind (right #t) neg neg neg))
     #t)
    (test-equal (either= eqv? (left #f) (either-bind (right #t) neg left neg))
     #t))

  ;; either-compose
  (test-equal (left-of-z? ((either-compose left) 'z))              #t)
  (test-equal (either= eqv? (right #t) ((either-compose right) #t))#t)

  ;; Compose with multiple mprocs.
  (let ((neg (lambda (b) (right (not b)))))
    (test-equal (either= eqv? (right #t) ((either-compose neg neg neg) #f))
     #t)))

;;;; Sequence operations

(define (check-sequence-operations)
  (define (both b c) (and b c))

  (test-equal (maybe-length (nothing))0)
  (test-equal (maybe-length (just #t))1)

  (test-equal (either-length (left #t)) 0)
  (test-equal (either-length (right #t))1)

  ;; maybe-filter & maybe-remove
  (test-equal (just-of-z? (maybe-filter always (just 'z)))#t)
  (test-equal (nothing? (maybe-filter never (just #t)))   #t)
  (test-equal (nothing? (maybe-filter always (nothing)))  #t)

  (test-equal (maybe= eqv? (just #t #t) (maybe-filter both (just #t #t)))#t)

  (test-equal (just-of-z? (maybe-remove never (just 'z)))#t)
  (test-equal (nothing? (maybe-remove always (just #t))) #t)
  (test-equal (nothing? (maybe-remove always (nothing))) #t)

  (test-equal (maybe= eqv? (just #t #f) (maybe-remove both (just #t #f)))#t)

  ;; maybe-sequence
  (test-equal (maybe= equal? (maybe-sequence (map just '(#t #f)) map identity)
                        (just '(#t #f)))
   #t)
  (test-equal (maybe= equal? (maybe-sequence (list (just 1 #t) (just 2 #f))
                                        map
                                        list)
                        (just '((1 #t) (2 #f))))
   #t)
  (test-equal (nothing? (maybe-sequence (list (just #t) (nothing)) map identity))
   #t)

  ;; either-filter & either-remove
  (test-equal (right-of-z? (either-filter always (right 'z) #f))#t)
  (test-equal (left-of-z? (either-filter never (right #t) 'z))  #t)
  (test-equal (left-of-z? (either-filter always (left #t) 'z))  #t)

  (test-equal (either= eqv? (right #t #t) (either-filter both (right #t #t) #f))
   #t)

  (test-equal (right-of-z? (either-remove never (right 'z) #f))#t)
  (test-equal (left-of-z? (either-remove always (right #t) 'z))#t)
  (test-equal (left-of-z? (either-remove never (left #t) 'z))  #t)

  (test-equal (either= eqv? (right #t #f) (either-remove both (right #t #f) #f))
   #t)

  ;; either-sequence
  (test-equal (either= equal? (either-sequence (map right (list 1 2)) map identity)
                         (right (list 1 2)))
   #t)
  (test-equal (left-of-z? (either-sequence (list (right #t) (left 'z)) map identity))
   #t)
  (test-equal (either= equal? (either-sequence (list (right 1 #t) (right 2 #f))
                                          map
                                          list)
                         (right '((1 #t) (2 #f))))
   #t))

;;;; Protocol conversion procedures

(define (check-conversions)
  (test-equal (maybe->list (nothing))     '())
  (test-equal (maybe->list (just #t #t))  '(#t #t))
  (test-equal (either->list (right #t #t))'(#t #t))
  (test-equal (either->list (left #t #t)) '(#t #t))

  (test-equal (nothing? (list->maybe '()))        #t)
  (test-equal (just-of-z? (list->maybe '(z)))     #t)
  (test-equal (left-of-z? (list->either '() 'z))  #t)
  (test-equal (right-of-z? (list->either '(z) #f))#t)

  (test-equal (maybe->truth (nothing))  #f)
  (test-equal (maybe->truth (just 'z))  'z)
  (test-equal (either->truth (left 'z)) #f)
  (test-equal (either->truth (right 'z))'z)

  (test-equal (nothing? (truth->maybe #f))       #t)
  (test-equal (just-of-z? (truth->maybe 'z))     #t)
  (test-equal (left-of-z? (truth->either #f 'z)) #t)
  (test-equal (right-of-z? (truth->either 'z #f))#t)

  (test-equal (maybe->list-truth (just 'z #t))  '(z #t))
  (test-equal (maybe->list-truth (nothing))     #f)
  (test-equal (either->list-truth (right 'z #t))'(z #t))
  (test-equal (either->list-truth (left 'z))    #f)

  (test-equal (just-of-z? (list-truth->maybe '(z)))  #t)
  (test-equal (nothing? (list-truth->maybe #f))      #t)
  (test-equal (right-of-z? (list-truth->either '(z)))#t)
  (test-equal (left-of-z? (list-truth->either #f 'z))#t)

  (test-equal (eof-object? (maybe->generation (nothing)))        #t)
  (test-equal (maybe->generation (just #t))                      #t)
  (test-equal (nothing? (generation->maybe (eof-object)))        #t)
  (test-equal (just-of-z? (generation->maybe 'z))                #t)

  (test-equal (eof-object? (either->generation (left)))        #t)
  (test-equal (either->generation (right #t))                  #t)
  (test-equal (left-of-z? (generation->either (eof-object) 'z))#t)
  (test-equal (right-of-z? (generation->either 'z #f))         #t)

  ;; maybe->values and friends
  (test-equal (maybe->values (just #t))               #t)
  (test-equal (values->list (maybe->values (nothing)))'())

  (test-equal (values->list (maybe->two-values (nothing)))       '(#f #f))
  (test-equal (values->list (maybe->two-values (just #t)))       '(#t #t))

  (test-equal (just-of-z? (two-values->maybe (lambda () (values 'z #t))))#t)
  (test-equal (nothing? (two-values->maybe (lambda () (values 'z #f))))  #t)

  (test-equal (nothing? (values->maybe (lambda () (values))))#t)
  (test-equal (just-of-z? (values->maybe (lambda () 'z)))    #t)
  (test-equal (maybe->values (values->maybe (lambda () #t))) #t)
  (test-equal (just-of-z? (values->maybe (lambda ()
                                      (maybe->values (just 'z)))))
   #t)

  ;; either->values and friends
  (test-equal (either->values (right #t))#t)
  (test-equal (values->list (either->values (left 'z)))'())

  (test-equal (left-of-z? (values->either (lambda () (values)) 'z))#t)
  (test-equal (right-of-z? (values->either (lambda () 'z) #f))     #t)
  (test-equal (either->values (values->either (lambda () #t) #f))  #t)
  (test-equal (right-of-z? (values->either (lambda ()
                                        (either->values (right 'z)))
                                      #f))
   #t)

  (test-equal (left-of-z? (exception->either symbol? (lambda () (raise 'z))))
  #t)
  (test-equal (right-of-z? (exception->either symbol? (lambda () 'z)))#t)
  (test-equal (guard (obj ((symbol? obj) obj))
           (exception->either number?
                              (lambda () (raise-continuable 'z))))
  'z)
;  (test-equal (either= eqv?
;                  (with-exception-handler
;                   not
;                   (lambda ()
;                     (exception->either string?
;                                        (lambda ()
;                                          (not (raise-continuable #t))))))
;                  (right #t))
;  #t)
  
  )

;;;; Map, fold, and unfold

(define (check-map-fold-and-unfold)
  ;; maybe-map
  (test-equal (nothing? (maybe-map not (nothing)))             #t)
  (test-equal (maybe= eqv? (just #f) (maybe-map not (just #t)))#t)

  (test-equal (maybe= eqv? (just #t #f) (maybe-map values (just #t #f)))#t)

  ;; either-map
  ;; Verify that the result is the same Left (in the sense of eqv?).
  (test-equal (let ((e (left #t))) (eqv? e (either-map not e)))    #t)
  (test-equal (either= eqv? (right #f) (either-map not (right #t)))#t)

  (test-equal (let ((e (right #t #f)))
           (either= eqv? e (either-map values e)))
   #t)

  ;; maybe-for-each
  (test-equal (let ((x #f))
           (maybe-for-each (lambda (y) (set! x y)) (just #t))
           x)
   #t)
  ; Given Nothing, ensure the proc argument is not executed.
  (test-equal (let ((x #f))
           (maybe-for-each (lambda (_) (set! x #t)) (nothing))
           x)
   #f)

  ;; either-for-each
  (test-equal (let ((x #f))
           (either-for-each (lambda (y) (set! x y)) (right #t))
           x)
   #t)
  ;; Given a Left, ensure the proc argument is not executed.
  (test-equal (let ((x #f))
           (either-for-each (lambda (_) (set! x #t)) (left 'z))
           x)
   #f)

  (test-equal (maybe-fold cons '() (nothing))'())
  (test-equal (maybe-fold cons '() (just #t))'(#t))
  (test-equal (maybe-fold * 2 (just 3 4))    24)

  (test-equal (either-fold cons '() (left #t)) '())
  (test-equal (either-fold cons '() (right #t))'(#t))
  (test-equal (either-fold * 2 (right 3 4))    24)

  (test-equal (nothing? (maybe-unfold always not always #f))          #t)
  (test-equal (maybe= eqv? (just #t) (maybe-unfold values not not #f))#t)
  (test-equal (maybe= eqv? (just #t 'z)
                      (maybe-unfold (lambda (b _) (not b))
                                    values
                                    (lambda (b x) (values (not b) x))
                                    #t
                                    'z))
  #t)

  (test-equal (left-of-z? (either-unfold always not always 'z))         #t)
  (test-equal (either= eqv? (right #t) (either-unfold values not not #f))#t)
  (test-equal (either= eqv? (right #t 'z)
                       (either-unfold (lambda (b _) (not b))
                                      values
                                      (lambda (b x) (values (not b) x))
                                      #t
                                      'z))
  #t))

;;;; Trivalent logic

(define (check-trivalent)
  (define (tri-true? m)
    (and (just? m) (maybe-ref/default m 'z)))

  (define (tri-false? m)
    (and (just? m) (not (maybe-ref/default m 'z))))

  (test-equal (tri-true? (tri-not (just #f))) #t)
  (test-equal (tri-false? (tri-not (just #t)))#t)
  (test-equal (nothing? (tri-not (nothing)))  #t)

  (test-equal (tri-true? (tri=? (just #t) (just 1) (just 'x)))#t)
  (test-equal (tri-true? (tri=? (just #f) (just #f)))         #t)
  (test-equal (tri-true? (tri=? (just #f) (just #f)))         #t)
  (test-equal (tri-false? (tri=? (just #f) (just #t)))        #t)
  (test-equal (tri-false? (tri=? (just #f) (nothing)))        #t)

  (test-equal (tri-true? (tri-and (just #t) (just 1) (just 'x)))#t)
  (test-equal (nothing? (tri-and (just #t) (nothing)))          #t)
  (test-equal (tri-false? (tri-and (just #f) (just #t)))        #t)
  (test-equal (tri-true? (tri-and))                             #t)

  (test-equal (tri-false? (tri-or (just #f) (just #f) (just #f)))#t)
  (test-equal (nothing? (tri-or (just #f) (nothing)))            #t)
  (let ((m-true (just 'x)))
    (test-equal (maybe= eqv? m-true (tri-or (just #f) m-true))   #t))
  (test-equal (tri-false? (tri-or))                              #t)

  (test-equal (nothing? (tri-merge (nothing) (nothing) (nothing))) #t)
  (let ((m-true (just 'x)))
    (test-equal (maybe= eqv? m-true (tri-merge (nothing) m-true))  #t))
  (let ((m-false (just #f)))
    (test-equal (maybe= eqv? m-false (tri-merge (nothing) m-false))#t))
  (test-equal (nothing? (tri-merge))                               #t))

(define (check-all)
  (check-constructors)
  (check-predicates)
  (check-accessors)
  (check-join-and-bind)
  (check-sequence-operations)
  (check-conversions)
  (check-map-fold-and-unfold)
  (check-trivalent))

(test-begin "srfi-189")
(check-all)
(test-end)
