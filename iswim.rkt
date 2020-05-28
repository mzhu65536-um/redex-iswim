#lang racket
(require redex)
(require "iswim-sugar.rkt")

(provide (all-defined-out))

;;; ISWIM core
(define-language iswim
  (e ::=
     x
     (λ x e)
     (e e)
     b
     (o2 e e)
     (o1 e))
  (o ::= o1 o2)
  (o1 ::= add1 sub1 zero?)
  (o2 ::= + - * expt)
  (b ::= number)
  (v ::= b X (λ x e))
  (x ::= variable-not-otherwise-mentioned)

  ;; cbv eval context
  (E ::= hole (v E) (E e) (o v ... E e ...)))

;;; --------------------------- ISWIM Substitution ---------------------------
(define-metafunction iswim
  subst : any x e -> any
  [(subst (λ x_1 any_1) x_1 any_2)
   (λ x_1 any_1)]
  [(subst (λ x_1 any_1) x_2 any_2)
   (λ x_3 (subst (subst-var any_1 x_1 x_3) x_2 any_2))
   (where x_3 ,(variable-not-in (term (x_2 any_1 any_2))
                                (term x_1)))]

  [(subst x_1 x_1 any_1) any_1]
  [(subst (any_2 ...) x_1 any_1)
   ((subst any_2 x_1 any_1) ...)]
  [(subst any_2 x_1 any_1) any_2])

(define-metafunction iswim
  ; subst-var : any x x -> any
  [(subst-var (any_1 ...) x_1 x_2)
   ((subst-var any_1 x_1 x_2) ...)]
  [(subst-var x_1 x_1 x_2) x_2]
  [(subst-var any_1 x_1 x_2) any_1])

(module+ test
  (test-equal
   (term (subst (zero? x) x 0))
   (term (zero? 0)))
  (test-equal
   (term (subst (λ x_1 (λ x_2 x_1)) x_1 2))
   (term (λ x_1 (λ x_2 x_1))))
  (test-equal
   (term (subst (λ x_1 (λ x_2 x_1)) x_3 2))
   (term (λ x_4 (λ x_2 x_4))))
  (test-equal
   (term (subst (λ x_1 (λ x_2 x_1)) x_2 2))
   (term (λ x_3 (λ x_2 x_3))))
  (test-equal
   (term (subst (λ x_1 (λ x_2 x_3)) x_3 2))
   (term (λ x_1 (λ x_2 2))))
  (test-equal
   (term (subst (λ x_1 (λ x_1 (x_1 x_2))) x_1 (λ x_1 x_2)))
   (term (λ x_1 (λ x_1 (x_1 x_2)))))
  (test-equal
   (term (subst (λ x_1 (λ x_1 (x_1 x_2))) x_2 (λ x_1 x_1)))
   (term (λ x_3 (λ x_4 (x_4 (λ x_1 x_1))))))
  )


(define-metafunction iswim
  δ : e -> v
  [(δ (zero? 0)) ,λ-t]
  [(δ (zero? b)) ,λ-f]
  [(δ (add1 b)) ,(add1 (term b))]
  [(δ (sub1 b)) ,(sub1 (term b))]
  [(δ (+ b_1 b_2)) ,(+ (term b_1) (term b_2))]
  [(δ (- b_1 b_2)) ,(- (term b_1) (term b_2))]
  [(δ (* b_1 b_2)) ,(* (term b_1) (term b_2))]  
  [(δ (expt b_1 b_2)) ,(expt (term b_1) (term b_2))])


(module+ test
  (test-equal (term (δ ,tm-zero?-0)) λ-t)
  (test-equal (term (δ (zero? 1))) λ-f)
  (test-equal (term (δ (+ 1 2))) 3)
  )

;;; ------------------------------ Reduction ---------------------------------
(define r/iswim
  (reduction-relation
   iswim #:domain e
   (--> ((λ x e) v)
        (subst e x v)
        r-β)
   (--> (o b ...)
        (δ (o b ...))
        r-δ)))

(module+ trace:r/iswim
  (require redex)
  (traces r/iswim Ω-comb)
  (traces r/iswim (term (add1 1))))

;; intermezzo : compatible-closure on "expression"
(define -->c/iswim
  (compatible-closure r/iswim iswim e))

(module+ trace:-->c/iswim
  (require redex)
  (traces -->c/iswim Ω-comb)
  (traces -->c/iswim (λ-if (term (zero? 1)) (term 2) (term 3))))

;; track : context-closure on "eval context"
(define -->r/iswim
  (context-closure r/iswim iswim E))

(module+ trace:-->r/iswim
  (require redex)
  (traces -->r/iswim Ω-comb)
  (traces -->r/iswim (λ-if (term (zero? 1)) (term 2) (term 3))))

;;; ----------------------------------- DONE -----------------------------------
(module+ test
  (test-results))
