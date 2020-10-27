;;; axe-iam.el --- Package for interacting with AWS IAM -*- lexical-binding: t; package-lint-main-file: "axe.el"; -*-

;; Copyright (C) 2020 Craig Niles

;; Author: Craig Niles <niles.c at gmail.com>
;; Maintainer: Craig Niles <niles.c at gmail.com>
;; URL: https://github.com/cniles/axe

;; This file is NOT part of GNU Emacs.

;; axe is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; axe is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; axe-iam.el provides functions for interacting with AWS IAM APIs.

;;; Commentary:

;; Provides API functions and interactive functions for AWS IAM.

;;; Code:

(require 'axe-api)
(require 'axe-buffer-mode)
(require 'a)

(cl-defun axe-iam--list-users (success &key marker max-items path-prefix)
  "Get list of users in the account and call SUCCESS when done."
  (axe-api-request
   "iam.amazonaws.com" 'iam
   success
   "GET"
   :query-params (rassq-delete-all nil
				   (a-list
				    "Action" "ListUsers"
				    "Version" "2010-05-08"
				    "PathPrefix" path-prefix
				    "Marker" marker
				    "MaxItems" max-items))
   :parser #'axe-util--xml-read))

;;;###autoload
(defun axe-iam-list-users ()
  "List all the IAM users for the current account."
  (interactive)
  (axe-list-api-results
   #'axe-iam--list-users
   "*axe-iam-users*"
   (cl-function
    (lambda (&key data &allow-other-keys)
      (mapcar
       (lambda (node)
	 (let ((member (axe-util--xml-node-to-alist node)))
	   (list nil (vector
		      (alist-get 'CreateDate member)
		      (alist-get 'PasswordLastUsed member "Never")
		      (list (format "%s" (alist-get 'UserName member)) 'member member)))))
       (axe-util--search-xml-children 'member data))))
   [("Creation Date" 20 t)
    ("Last Login" 20 t)
    ("User Name" 1 t)]
   nil
   nil
   (cl-function
    (lambda (&key data &allow-other-keys)
      (let ((result (axe-util--xml-node-to-alist (axe-util--search-xml-children 'ListUserResponse data))))
	(if (not (null (alist-get 'IsTruncated result nil nil)))
	    (alist-get 'Marker result nil nil)))))
   :auto-follow t
   :auto-follow-delay 0.0))

(provide 'axe-iam)
;;; axe-iam.el ends here
