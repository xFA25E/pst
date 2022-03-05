test: pds-test.el
	emacs -Q -module-assertions -batch -L . $(foreach test,$<,-l $(test)) -f ert-run-tests-batch-and-exit
