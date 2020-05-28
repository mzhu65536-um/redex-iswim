#lang racket
(require redex)
(provide (all-defined-out))

;;; helper
(define (hole? t)
  (equal? (term hole) t))

;;; sugar
(define (λ-if t1 t2 t3)  
  (term ((((λ x (λ y (λ z ((x y) z)))) ,t1) ,t2) ,t3)))

(define λ-t
  (term (λ x_1 (λ x_2 x_1))))
(define λ-f
  (term (λ x_1 (λ x_2 x_2))))

(define Ω-comb
  (term ((λ y (y y)) (λ x (x x)))))

(define Ω-comb-loop
  (term ((λ x (x x)) (λ x (x x)))))

(define example-sub1-if-0
  (term (λ x ,(λ-if (term (zero? x)) (term (sub1 x)) (term x)))))

(define tm-zero?-0
  (term (zero? 0)))
