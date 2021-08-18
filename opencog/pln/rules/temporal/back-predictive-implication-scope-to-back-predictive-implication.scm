;; BackPredictiveImplicationScope to BackPredictiveImplication Rule 
;; (lookback variant)
;;
;; BackPredictiveImplicationScope <TV>
;;   V
;;   L
;;   Evaluation  |
;;     P         | P-body
;;     V         '
;;   Evaluation  |
;;     P         | Q-body
;;     V         '
;; <=>
;; BackPredictiveImplication <TV>
;;   L
;;   Lambda
;;     V T
;;     AtTime
;;       Evaluation
;;         P
;;         V
;;       T
;;   Lambda
;;     V T
;;     AtTime
;;       Evaluation
;;         Q
;;         V
;;       T
;; where L is a time lag, P and Q are temporal predicates.

(use-modules (opencog))
(use-modules (opencog exec))
(use-modules (opencog spacetime))
(use-modules (opencog ure))
(use-modules (opencog pln))
(use-modules (opencog logger))

;; Rule
(define back-predictive-implication-scope-to-back-predictive-implication-rule
  (let* ((V (Variable "$V"))
      (L (Variable "$lag"))
	    (P (Variable "$P"))
	    (Q (Variable "$Q"))
      (T (Variable "$T"))
      (VardeclT (TypeChoice 
        (Type 'VariableNode)
       ; (Type 'VariableSet)
       ; (Type 'VariableList)
        (Type 'TypedVariableLink)))
	  (NaturalT (TypeInh 'NaturalLink))
	  (BPIS (Quote (BackPredictiveImplicationScope 
      (Unquote V) (Unquote L) (Unquote P) (Unquote Q))))
	  (BPI (List L 
      (List (VariableList V T) (AtTime P T))
      (List (VariableList V T) (AtTime Q T)))))
    (Bind
      (VariableSet
        (TypedVariable V VardeclT)
        (TypedVariable L NaturalT)
        P
        Q)
      (Present BPIS)
      (ExecutionOutput
        (GroundedSchema "scm: back-predictive-implication-scope-to-back-predictive-implication")
      (List
        ;; Conclusion
        BPI
        ;; Premises
        BPIS)))))

;; Rewrite conclusion
(define (rewrite-conclusion BPI)
  (let* ((body (cog-outgoing-set BPI))
        (L (car body))
        (Impt (Lambda (cog-outgoing-set (cadr body))))
        (Impd (Lambda (cog-outgoing-set (caddr body)))))
  (BackPredictiveImplication L Impt Impd)))

;; Formula
(define (back-predictive-implication-scope-to-back-predictive-implication conclusion . premises)
  (ure-logger-fine "(back-predictive-implication-scope-to-back-predictive-implication conclusion[~x]=~a . premises=~a)" 
                    (cog-handle conclusion) conclusion premises)
  (if (= (length premises) 1)
      (let* ((BPIS (car premises))
            (BPI (rewrite-conclusion conclusion)))
	    (cog-merge-hi-conf-tv! BPI (cog-tv BPIS)))))

;; Declaration
(define back-predictive-implication-scope-to-back-predictive-implication-rule-name
  (DefinedSchemaNode "back-predictive-implication-scope-to-back-predictive-implication-rule"))
(DefineLink back-predictive-implication-scope-to-back-predictive-implication-rule-name
  back-predictive-implication-scope-to-back-predictive-implication-rule)
