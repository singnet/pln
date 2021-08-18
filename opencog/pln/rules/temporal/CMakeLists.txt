# Extensional rules
ADD_GUILE_MODULE(FILES
	predictive-implication-scope-direct-introduction.scm
	predictive-implication-direct-evaluation.scm
	predictive-implication-scope-direct-evaluation.scm
	predictive-implication-scope-deduction.scm
	back-predictive-implication-direct-evaluation.scm
	back-predictive-implication-scope-direct-evaluation.scm
	back-predictive-implication-scope-direct-introduction.scm
	back-predictive-implication-scope-to-back-predictive-implication.scm
	back-predictive-implication-to-implication.scm
	back-predictive-implication-scope-deduction-cogscm.scm
	utils.scm
	lagged-introduction.scm
	MODULE_DESTINATION "${GUILE_SITE_DIR}/opencog/pln/rules/temporal"
)
