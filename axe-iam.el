;;; axe-iam.el --- IAM API and interactions -*- lexical-binding: t; package-lint-main-file: "axe.el" -*-

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

;; You should have received a copy of the GNU General Public License
;; along with axe.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; axe-iam.el provides functions for interacting with AWS S3 APIs.  For
;; example, listing users and roles.

;;; Code:

(require 'axe-api)
(require 'axe-buffer-mode)

(cl-defun axe-iam--list-users (success &key next-token max-items path-prefix)
  "Call SUCCESS with the results of the IAM ListUsers API.
NEXT-TOKEN specifies the start of the next page of
results (Marker) and should be set to a value provided in a
previous ListUsers response.  MAX-ITEMS is used when paginating
results.  PATH-PREFIX is used to filter results.  See:
`https://docs.aws.amazon.com/IAM/latest/APIReference/API_ListUsers.html'"
  (axe-api-request
   "iam.amazonaws.com"
   'iam success
   "GET"
   :parser #'axe-util--xml-read
   :query-params (rassq-delete-all nil `(("Action" . "ListUsers")
					 ("Version" . "2010-05-08")
					 ("MaxItems" . ,max-items)
					 ("Marker" . ,next-token)))))

;;;###autoload
(defun axe-iam-list-users ()
  "List IAM users in a buffer."
  (interactive)
  (axe-list-api-results
   #'axe-iam--list-users
   "*axe-iam-users-list*"
   (cl-function
    (lambda (&key data &allow-other-keys)
      (mapcar
       (lambda (node)
	 (let ((user (axe-util--xml-node-to-alist node)))
	   (list nil (vector
		      (format "%-30s" (alist-get 'CreateDate user))
		      (alist-get 'UserName user)))))
       (axe-util--search-xml-children 'member data))))
   [("Creation Date" 30 t) ("User Name" 1 t)]
   ()
   ()
   (cl-function
    (lambda (&key data &allow-other-keys)
      (let ((result (axe-util--xml-node-to-alist (first (axe-util--search-xml-children 'ListUsersResult data)))))
	(alist-get 'Marker result))))))

(provide 'axe-iam)
;;; axe-iam.el ends here
