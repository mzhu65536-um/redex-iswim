#lang racket
(require redex
         "iswim-sugar.rkt"
         "iswim.rkt")
(provide (all-defined-out))

;;; ------------------------------ ISWIM/err ---------------------------------
(define-extended-language iswim/err iswim
  (e ::= .... (err b))                  ; label can be *any*thing
  (o2 ::= .... /))                      

(define-metafunction/extension δ iswim/err
  δ/ : e -> e
  [(δ/ (/ b 0)) (err 0)]
  [(δ/ (/ b_1 b_2)) ,(/ (term b_1) (term b_2))])

;; β + δ + δ\ 
(define r/iswim/err
  (extend-reduction-relation
   r/iswim
   iswim/err
   (--> (o b ...) (δ/ (o b ...)) r-δ)
   (--> (o b ... (λ x e) v ...) (err ,(add1 (length (term (b ...)))))
        r-err-o-λ)
   (--> (b v ...) (err b)
        r-err-b-v)))

;; lift (err b) in e
(define abort
  (reduction-relation
   iswim/err
   (--> (in-hole E (err b))
        (err b)
        (side-condition (not (equal? (term hole) (term E))))
        abort)))

;; E(β + δ + δ\) + abort
(define -->r/iswim/err
  (union-reduction-relations
   abort
   (context-closure r/iswim/err iswim/err E)))

(module+ test
  (define (test:-->r/iswim/err a b)
    (test-->> -->r/iswim/err #:equiv equal? a b))
  
  (test-equal (term (δ/ (/ 4 2))) (term 2))
  (test-equal (term (δ/ (/ 4 0))) (term (err 0)))
  (test:-->r/iswim/err (term (/ 4 2)) (term 2))
  (test:-->r/iswim/err
   (term (+ 1 (+ 2 (+ 3 (λ x 1)))))
   (term (err 2)))
  (test-->>∃ #:steps 2
             -->r/iswim/err
             Ω-comb
             Ω-comb-loop)
  (test-results))

(module+ trace:-->r/iswim/err
  (require redex)
  #;(traces -->r/iswim/err Ω-comb)
  (test-->> -->r/iswim/err #:equiv equal?  (term (/ 4 2)) (term 2))
  (traces -->r/iswim/err (term (/ 1 0)))
  (traces -->r/iswim/err (term (+ 1 (+ 2 (+ 3 (λ x 1))))))
  (traces -->r/iswim/err
          (term (/
                 ((λ x
                    (+ (/ 1 x) (err 2)))
                  7)
                 2))))

;;; ----------------------------------- DONE -----------------------------------
(module+ test
  (test-results))
