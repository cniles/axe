;;; axe-util-test.el --- Unit tests for axe-util.el -*- lexical-binding: t; -*-

;;; Code:

;;; Commentary:

(require 'axe-util)
(require 'ert)

(ert-deftest axe-util-test-extract-table-list-from-xml ()
  (let ((data (with-temp-buffer (insert-file-contents "test/simple.xml") (axe-util--xml-read)))
	(fn (axe-util--xml-to-table-list simple foo (bar :default "-") baz)))
    (should (equal '((nil ["Foo 1" "Bar 1" "Baz 1"])
		     (nil ["Foo 2" "Bar 2" "Baz 2"])
		     (nil ["Foo 3" "-" "Baz 3"]))
		   (funcall fn :data data)))))

(provide 'axe-util-test)
;;; axe-util-test.el ends here
