;;; axe-api-test.el --- Tests for axe-api.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for AWS sigv4 subroutines

;;; Code:

(require 'axe-api)

(ert-deftest axe-api-test--make-signing-key ()
  "Tests the output of make-signing-key"
  (should (equal (axe-util--hexify
		  (axe-api--sigv4-make-signing-key "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY" (encode-time 0 0 0 15 02 2012) 'us-east-1 'iam))
		 "F478E2D9F65FA895F9C67B32CE1BAF0B0D8A4355A00A1A9E090D414DB404D")))

(provide 'axe-api-test)
;;; axe-api-test.el ends here
