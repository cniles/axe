CASK_COMMAND ?= cask

dependencies:
	@echo " * Install and update axe dependencies * "
	cask install
	cask update

build: dependencies
	@echo " * Compiling elc * "
	cask build

test: build
	@echo " * Running axe tests * "
	@$(CASK_COMMAND) emacs -Q -batch \
		-l axe-api-test.el \
		-f ert-run-tests-batch-and-exit

dist: dependencies build test
	@echo " * Packaging axe * "
	cask package archive_contents/

clean:
	@echo " * Cleaning files * "
	rm -rf dist
	cask clean-elc

.PHONY: dependencies build test clean
