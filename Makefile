CASK_COMMAND ?= cask

dependencies:
	@echo " * Install and update axe dependencies * "
	$(CASK_COMMAND) install
	$(CASK_COMMAND) update

build: dependencies
	@echo " * Compiling elc * "
	$(CASK_COMMAND) build

test: build
	@echo " * Running axe tests * "
	@EMACSLOADPATH=.: $(CASK_COMMAND) emacs -Q -batch \
		-l axe-api-test.el \
		-f ert-run-tests-batch-and-exit

lint: build
	@echo " * Running package-lint * "
	sh lint.sh

dist: dependencies build test
	@echo " * Packaging axe * "
	$(CASK_COMMAND) package archive_contents/

clean:
	@echo " * Cleaning files * "
	rm -rf dist
	$(CASK_COMMAND) clean-elc

.PHONY: dependencies build test clean
