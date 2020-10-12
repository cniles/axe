INIT_PACKAGE_EL="(progn \
  (require 'package)
  (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) \
  (package-initialize) \
  (unless package-archive-contents \
    (package-refresh-contents)))"

cask emacs -Q --batch \
     -l package-lint \
     --eval "$INIT_PACKAGE_EL" \
     --eval '(setq package-lint-main-file "axe.el")' \
     -f package-lint-batch-and-exit \
     axe*.el

CHECKDOC_FN="(defun checkdoc-batch () \
(mapcar #'checkdoc-file command-line-args-left))"

cask emacs -Q --batch \
     --eval "$CHECKDOC_FN" \
     -f checkdoc-batch \
     axe-*.el
