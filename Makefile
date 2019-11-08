.PHONY: help test clean format disclean


ELM := elm
ELM_FORMAT := elm-format
ELM_TEST := elm-test
ELM_VERIFY_EXAMPLES := elm-verify-examples
RIMRAF := rm -rf


# The default goal
help:
	@echo 'Available commands:'
	@echo '  help'
	@echo '  test'
	@echo '  format'
	@echo '  clean'
	@echo '  distclean'


test: format tests/VerifyExamples
	$(ELM_TEST) --compiler=$$(which $(ELM))


format: clean
	$(ELM_FORMAT) --validate .


tests/VerifyExamples:
	$(ELM_VERIFY_EXAMPLES)


clean:
	$(RIMRAF) tests/VerifyExamples


distclean: clean
	$(RIMRAF) node_modules
