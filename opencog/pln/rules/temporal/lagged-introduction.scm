;; LaggedLink introduction rule
;;
;; L
;; P <TV>
;; |-
;; Lagged <TV>
;;   P
;;   L
;;
;; where L is a time lag and P is a temporal predicate.

(use-modules (opencog))
(use-modules (opencog exec))
(use-modules (opencog spacetime))
(use-modules (opencog ure))
(use-modules (opencog pln))
(use-modules (opencog logger))

;; Rule
(define lagged-introduction-rule
  (let* ((L (Variable "$lag"))
	    (P (Variable "$P"))
	    (NaturalT (TypeInh 'NaturalLink))
      (Pred (TypeChoice (Type 'PredicateNode) (Type 'LambdaLink)))
      (LL (Lagged P L)))
    (Bind
      (VariableSet
        (TypedVariable L NaturalT)
        (TypedVariable P Pred))
      (Present P L)
      (ExecutionOutput
        (GroundedSchema "scm: lagged-introduction")
      (List
        ;; Conclusion
        LL
        ;; Premises
        P)))))

;; Formula
(define (lagged-introduction conclusion . premises)
  (ure-logger-fine "(lagged-introduction conclusion[~x]=~a . premises=~a)" (cog-handle conclusion) conclusion premises)
  (if (= (length premises) 1)
      (let* ((P (car premises)))
	  (cog-merge-hi-conf-tv! conclusion (cog-tv P)))))

;; Declaration
(define lagged-introduction-rule-name
  (DefinedSchemaNode "lagged-introduction-rule"))
(DefineLink lagged-introduction-rule-name
  lagged-introduction-rule)
