;;; axe-util-test.el --- Unit tests for axe-util.el -*- lexical-binding: t; -*-

;;; Code:

;;; Commentary:

(require 'axe-util)
(require 'ert)

(ert-deftest axe-util-test-extract-table-list-from-xml ()
  (let ((data (with-temp-buffer (insert-file "test/simple.xml") (axe-util--xml-read))))
    (cl-flet ((fn (axe-util--xml-to-table-list 
    (insert (format "%s" data))
    (insert (
	      
(provide 'axe-util-test)
;;; axe-util-test.el ends here
