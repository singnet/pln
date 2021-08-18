;; BackPredictiveImplication to Implication Rule 
;; (lookback variant)
;;
;; BackPredictiveImplication <TV>
;;   L
;;   P
;;   Q
;; <=>
;; Implication <TV>
;;   Lagged
;;     P
;;     L
;;   BackSequentialAnd
;;     L
;;     P
;;     Q
;;
;; where L is a time lag, P and Q are temporal predicate bodies.

(use-modules (opencog))
(use-modules (opencog exec))
(use-modules (opencog spacetime))
(use-modules (opencog ure))
(use-modules (opencog pln))
(use-modules (opencog logger))

;; Rule
(define back-predictive-implication-to-implication-rule
  (let* ((L (Variable "$lag"))
	  (P (Variable "$P"))
	  (Q (Variable "$Q"))
	  (NaturalT (TypeInh 'NaturalLink))
	  (BPI (BackPredictiveImplication L P Q))
	  (Im (Implication (Lagged P L) (BackSequentialAnd L P Q))))
    (Bind
      (VariableSet
        (TypedVariable L NaturalT)
        P
        Q)
      (Present BPI)
      (ExecutionOutput
        (GroundedSchema "scm: back-predictive-implication-to-implication")
      (List
        ;; Conclusion
        Im
        ;; Premises
        BPI)))))

;; Formula
(define (back-predictive-implication-to-implication conclusion . premises)
  (ure-logger-fine "(back-predictive-implication-to-implication conclusion[~x]=~a . premises=~a)" (cog-handle conclusion) conclusion premises)
  (if (= (length premises) 1)
      (let* ((BPI (car premises)))
	  (cog-merge-hi-conf-tv! conclusion (cog-tv BPI)))))

;; Declaration
(define back-predictive-implication-to-implication-rule-name
  (DefinedSchemaNode "back-predictive-implication-to-implication-rule"))
(DefineLink back-predictive-implication-to-implication-rule-name
  back-predictive-implication-to-implication-rule)
