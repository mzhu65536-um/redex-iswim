#lang racket
(require redex
         "iswim-sugar.rkt"
         "iswim-err.rkt")
(provide (all-defined-out))

;;; ---------------------------------------------------------------------- ;;;
;;; ISWIM/err+handler
(define-extended-language iswim/err+handler
  iswim/err
  (e ::= ....
     (throw e)                          ; why not allow reduction in [throw]?
     (catch e (λ x e)))
  (E ::= .... (throw E))                ; extend eval context to support [throw]
  (H ::= E
     (catch H (λ x e))))
;;; the Redex book defined throw to be (throw b); but, I extend the definition


(define error->throw                    ; error becomes throw 
  (reduction-relation
   iswim/err+handler
   #:domain e
   #:codomain e   
   (--> (err b) (throw b) error->throw)))

(define catch
  (reduction-relation
   iswim/err+handler
   (--> (catch v (λ x e)) v catch-normal)
   (--> (catch (throw b) (λ x e)) ((λ x e) b) catch-throw)))

; β + δ + δ\ + catch 
(define h/iswim/err+handler
  (union-reduction-relations
   catch
   error->throw
   (extend-reduction-relation
    r/iswim/err iswim/err+handler
    (--> (o b ...) (δ/ (o b ...)) r-δ))))

;  E(β + δ + δ/ + catch)
(define -->h/iswim/err+handler
  (context-closure h/iswim/err+handler iswim/err+handler E))

; [E(e)] -> [e]
(define throw
  (reduction-relation 
   iswim/err+handler
   (--> (in-hole H
                 (catch (in-hole E (throw b)) 
                   (λ x e)))
        (in-hole H
                 (catch (throw b) 
                   (λ x e)))
        (side-condition (not (hole? (term E))))
        H-catch-lift-throw)
   (--> (in-hole E (throw b)) (throw b)
        (side-condition (not (hole? (term E))))
        E-lift-throw)))


; throw + H(E(β + δ + δ/ + catch)
(define -->r/iswim/err+handler
  (union-reduction-relations
   throw
   (context-closure -->h/iswim/err+handler iswim/err+handler H)))


(module+ test
  (define-syntax (test--> stx)
    (syntax-case stx ()
      [(_ a b)
       #'(test-->> -->r/iswim/err+handler #:equiv equal? (term a) (term b))]
      [_ (error "illegal syntax")]))

  (test-->
   (catch (catch (+ 1 (+ 2 (throw 3))) (λ x x)) (λ x 4))
   3)

  (test-->
   (catch (catch (+ 1 (/ 1 0)) (λ x x)) (λ x 4))
   0)

  ;;; unsupported in Redex book: you cannot throw an exp
  (test-->
   (catch
       (catch (+ 1 (/ 1 0)) (λ x (throw (add1 x))))
     (λ x (throw (add1 x))))
   (throw 2))

  (test-->
   ((λ x (catch
             (catch (+ 1 (+ x (/ 2 x))) (λ y (add1 x)))
           (λ y x)))
    0)
   1)
  (test-->
   ((λ x (catch
             (catch (+ 1 (+ x (/ 2 x))) (λ y (add1 x)))
           (λ y x)))
    1)
   4)

  (test-results))

(module+ trace:-->r/iswim/err+handler
  (require redex)

  (define-syntax (traces-datum tm)
    (syntax-case tm ()
      [(_ tm) #'(traces -->r/iswim/err+handler (term tm))]))
  
  (traces-datum ,Ω-comb)
  (traces-datum
   (catch
       (catch (+ 1 (+ 2 (throw 2))) (λ x x))
     (λ x (add1 x))))
  (traces-datum (catch (/ 1 0) (λ x (add1 x))))
  (traces-datum (+ (+ 1 (+ 2 (throw 10)) 2)))
  )


#|
(module+ test
  (test-equal
   (term (subst (catch
                    (catch throw 1 with (λ x x))
                  with (λ x x))
                x 10))
   (term (catch
             (catch throw 1 with (λ x x))
           with (λ x x))))

  (test-equal
   (term (subst (catch
                    (catch (throw 1) with (λ x x))
                  with (λ x x))
                y 10))
   (term (catch
             (catch (throw 1) with (λ x1 x1))
           with (λ x1 x1))))
  
  (test-equal
   (term (subst (catch 1 with (λ x x)) x 10))
   (term (catch 1 with (λ x x)))))

(module+ trace-iswim/err+handler-std-red
  (require redex)
  (define (traces-it exp)
    (traces iswim/err+handler-std-red exp))
  (traces-it (term (catch (catch (throw 2) with (λ x x)) with (λ x (add1 x)))))
  (traces-it (term (catch (/ 1 0) with (λ x (add1 x)))))
  (traces-it (term (+ (throw 10) 2))))

;;; ISWIM/err+handler+control



;;; ISWIM/err+state
(define-extended-language iswim/err+state
  iswim/err
  (e ::= .... (set x e)))

(define-metafunction iswim/err+state
  av : e -> (x ...)
  [(av b) ()]
  [(av x) ()]
  [(av (err l)) ()]
  [(av (λ x e)) (x- (av e) x)]
  [(av (set x e)) (xu (av e) (x))]
  [(av (e_1 e_2)) (xu (av e_1) (av e_2))]
  [(av (o e ...)) (xu (av e) ...)])

(module+ test+
  (test-equal
   (term (av ((set x (λ x 1)) (set y 1))))
   (term (x y)))
  (test-equal
   (term (av (err λ)))
   (term ()))
  (test-equal
   (term (av (λ x (set x 1))))
   (term ())))

|#
