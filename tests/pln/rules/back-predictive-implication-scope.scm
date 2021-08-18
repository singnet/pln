;; Define example BackPredictiveImplicationScope to test the following rules
;;  - back-predictive_implication_scope_to_back-predictive_implication 
;;  - back-predictive_implication_to_implication
;; 

(BackPredictiveImplicationScopeLink (stv 0.345 0.091941)
  (VariableNode "$X")
  (SLink
    (ZLink))
  (EvaluationLink
    (PredicateNode "P")
    (VariableNode "$X"))
  (EvaluationLink
    (PredicateNode "Q")
    (VariableNode "$X")))

;; This one is to test lagged-introduction-rule
(PredicateNode "P" (stv 1 1))
(PredicateNode "Q" (stv 1 1)) 
