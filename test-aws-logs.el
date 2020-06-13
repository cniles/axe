;;; test-aws-logs -- Variables for testing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'aws-logs)
(require 'aws-util)
(require 'env)

(ert-deftest test-get-log-groups ()
  (aws-logs--describe-log-groups
   (lambda (response)
     (aws-sdk-log (assoc 'logGroups response)))
   :limit 2))

(ert-deftest test-get-log-groups ()
  (aws-logs-describe-log-groups ()))

(provide 'test)
;;; test-aws-logs.el ends here
