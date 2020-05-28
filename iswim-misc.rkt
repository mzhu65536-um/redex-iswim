#lang racket
(require redex
         "iswim.rkt")

;;; iswim free variables
(define-metafunction iswim
  fv : e -> (x ...)
  [(fv b) ()]
  [(fv x) (x)]
  [(fv (λ x e)) (x- (fv e) x)]
  [(fv (e_1 e_2)) (xu (fv e_1) (fv e_2))]
  [(fv (o e ...)) (xu (fv e) ...)]
  )

(define-metafunction iswim
  x- : (x ...) x -> (x ...)
  [(x- (x_1 ... x_2 x_3 ...) x_2)
   (x- (x_1 ... x_3 ...) x_2)
   (side-condition (not (memq (term x_2) (term (x_3 ...)))))]
  [(x- (x_1 ...) x_2) (x_1 ...)])

(define-metafunction iswim
  xu  : (x ...) ... -> (x ...)
  [(xu (x_1 ...) (x_2 ...) (x_3 ...) ...)
   (xu (x_1 ... x_2 ...) (x_3 ...) ...)]
  [(xu (x_1 ...)) (x_1 ...)]
  [(xu ()) ()])

(module+ test+
  (test-equal (term (fv 0)) (term ()))
  (test-equal (term (fv x)) (term (x)))
  (test-equal (term (fv (λ x x))) (term ()))
  (test-equal (term (x- (x x) x)) (term ()))
  (test-equal (term (fv ((λ y z) (λ x y)))) (term (z y)))
  (test-equal (term (fv (+ ((λ a b) (λ b c)) ((λ x y) (λ y z))))) (term (b c y z))))

