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
;; example, listing buckets and their contents.

;;; Code:

(require 'axe-api)

(cl-defun axe-iam--list-users (success &key marker max-items path-prefix)
  "Call SUCCESS with the results of the IAM ListUsers API.
MARKER specifies the start of the next page of results and should
be set to a value provided in a previous ListUsers response.
MAX-ITEMS is used when paginating results.  PATH-PREFIX is used
to filter results.  See:
`https://docs.aws.amazon.com/IAM/latest/APIReference/API_ListUsers.html'"
  (axe-api-request
   "iam.amazonaws.com"
   'iam success
   "GET"
   :parser #'axe-util--xml-read
   :query-params (rassq-delete-all nil `(("PathPrefix" . ,path-prefix)
					 ("Action" . "ListUsers")
					 ("Version" . "2010-05-08")))))

(provide 'axe-iam)
;;; axe-iam.el ends here
