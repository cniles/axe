;;; axe-s3.el --- S3 API and interactions -*- lexical-binding: t; package-lint-main-file: "axe.el" -*-

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

;; axe-s3.el provides functions for interacting with AWS S3 APIs.  For
;; example, listing buckets and their contents.

;;; Code:

(require 'axe-api)
(require 'axe-util)
(require 'axe-buffer-mode)
(require 'xml)
(require 'xmlgen)

(defun axe-s3--list-buckets (success)
  "Get buckets in the account and call SUCCESS when done."
  (axe-api-request
   "s3.amazonaws.com" 's3
   success
   "GET"
   :parser #'axe-util--xml-read
   :headers (list (cons "x-amz-content-sha256" (axe-api--sigv4-hash "")))))

(cl-defun axe-s3--list-objects-v2 (success bucket &key next-token delimiter encoding-type fetch-owner max-keys prefix start-after request-payer)
  "Call SUCCESS with result of List Object V2 on BUCKET.
Optional parameters NEXT-TOKEN, DELIMITER, ENCODING-TYPE,
MAX-KEYS, FETCH-OWNER, PREFIX, START-AFTER and REQUEST-PAYER
correspond to AWS API parameters.  NEXT-TOKEN corresponds to the
`continuation-token' request parameter."
  (axe-api-request
   (format "%s.s3.amazonaws.com" bucket) 's3
   success
   "GET"
   :parser #'axe-util--xml-read
   :query-params (rassq-delete-all nil `(("list-type" . "2")
					 ("continuation-token" . ,next-token)
					 ("delimiter" . ,delimiter)
					 ("encoding-type" . ,encoding-type)
					 ("fetch-owner" . ,fetch-owner)
					 ("max-keys" . ,max-keys)
					 ("prefix" . ,prefix)
					 ("start-after" . ,start-after)))
   :headers (rassq-delete-all nil `(("x-amz-content-sha256" . ,(axe-api--sigv4-hash ""))
				    ("x-amz-request-payer" . ,request-payer)))))

(cl-defun axe-s3--create-bucket (success bucket &key acl grant-full-control grant-read grant-read-acp grant-write grant-write-acp object-lock-enabled-for-bucket location-constraint)
  "Create bucket with name BUCKET invoking callback SUCCESS with the result.
ACL, GRANT-FULL-CONTROL GRANT-READ GRANT-READ-ACP GRANT-WRITE
GRANT-WRITE-ACP OBJECT-LOCK-ENABLED-FOR-BUCKET and
LOCATION-CONSTRAINT parameters behave as described in API
CreateBucket documentation.  See:
`https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateBucket.html'"
  (let ((payload (if location-constraint
		     (xmlgen `(CreateBucketConfiguration :xmlns "https://s3.amazonaws.com/doc/2006-03-01/"
							 (LocationConstraint location-constraint)))
		   "")))
    (axe-api-request
     (format "%s.s3.amazonaws.com" bucket)
     's3
     success
     "PUT"
     :parser #'axe-util--xml-read
     :headers (rassq-delete-all nil `(("Content-Type" . "text/plain")
				      ("x-amz-acl" . ,acl)
				      ("x-amz-grant-full-control" . ,grant-full-control)
				      ("x-amz-grant-read" . ,grant-read)
				      ("x-amz-grant-read-acp" . ,grant-read-acp)
				      ("x-amz-grant-write" . ,grant-write)
				      ("x-amz-grant-write-acp" . ,grant-write-acp)
				      ("x-amz-bucket-object-lock-enabled" . ,object-lock-enabled-for-bucket)
				      ("x-amz-content-sha256" . ,(axe-api--sigv4-hash payload))))
     :request-payload payload)))

(cl-defun axe-s3--delete-bucket (success bucket)
  "Create bucket with name BUCKET invoking callback SUCCESS with the result.
ACL, GRANT-FULL-CONTROL GRANT-READ GRANT-READ-ACP GRANT-WRITE
GRANT-WRITE-ACP OBJECT-LOCK-ENABLED-FOR-BUCKET and
LOCATION-CONSTRAINT parameters behave as described in API
CreateBucket documentation.  See:
`https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateBucket.html'"
  (axe-api-request
   (format "%s.s3.amazonaws.com" bucket)
   's3
   success
   "DELETE"
   :parser #'axe-util--xml-read
   :headers (rassq-delete-all nil `(("x-amz-content-sha256" . ,(axe-api--sigv4-hash ""))))))

(cl-defun axe-s3--get-object (success bucket key)
  "Download the contents of object specified by KEY from BUCKET.
Callback SUCCESS is invoked with the response.  See:
`https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html'"
  (axe-api-request
   (format "%s.s3.amazonaws.com" bucket)
   's3
   success
   "GET"
   :parser #'buffer-string
   :path-segments `(,key)
   :headers (rassq-delete-all nil `(("x-amz-content-sha256" . ,(axe-api--sigv4-hash ""))))))

;;;###autoload
(defun axe-s3-list-buckets ()
  "Lists buckets for the account into a buffer."
  (interactive)
  (axe-list-api-results
   #'axe-s3--list-buckets
   "*axe-s3-bucket-list*"
   (cl-function
    (lambda (&key data &allow-other-keys)
      (mapcar
       (lambda (node)
	 (let ((bucket (axe-util--xml-node-to-alist node)))
	   (list nil (vector
		      (format "%-30s" (alist-get 'CreationDate bucket))
		      (list (format "%s" (alist-get 'Name bucket)) 'bucket bucket)))))
       (axe-util--search-xml-children 'Bucket data))))
   [("Creation Date" 30 t) ("Bucket Name" 1 t)]
   ()
   (lambda (map)
     (define-key map (kbd "l") #'axe-s3-list-objects-in-bucket-at-point)
     map)
   ()))

;;;###autoload
(defun axe-s3-list-objects-in-bucket-at-point ()
    "List contents of bucket described at from text property or text."
    (interactive)
    (let ((bucket (axe-util--thing-or-property-near-point 'bucket 'Name)))
      (axe-s3-list-objects-v2 bucket)))

;;;###autoload
(defun axe-s3-list-objects-v2 (bucket)
  "List objects in BUCKET into a buffer."
  (interactive "sBucket: ")
  (axe-list-api-results
   #'axe-s3--list-objects-v2
   (format "*axe-s3-object-list:%s*" bucket)
   (cl-function
    (lambda (&key data &allow-other-keys)
      (mapcar
       (lambda (node)
	 (let ((content (axe-util--xml-node-to-alist node)))
	   (list nil (vector
		      (format "%-8s" (file-size-human-readable (string-to-number (alist-get 'Size content))))
		      (list (format "%s" (alist-get 'Key content)) 'content content)))))
       (axe-util--search-xml-children 'Contents data))))
   [("Size" 8 t) ("Key" 1 t)]
   `(,bucket)
   ()
   (cl-function
    (lambda (&key data &allow-other-keys)
      (let ((next-token (alist-get 'NextContinuationToken (axe-util--xml-node-to-alist data))))
	next-token)))))

;;;###autoload
(defun axe-s3-create-bucket (bucket)
  "Create BUCKET."
  (interactive "sBucket: ")
  (axe-s3--create-bucket
   (cl-function (lambda (&key &allow-other-keys) (message "Successfully created bucket %s" bucket)))
   bucket))

;;;###autoload
(defun axe-s3-delete-bucket (bucket)
  "Delete BUCKET."
  (interactive "sBucket: ")
  (axe-s3--delete-bucket
   (cl-function (lambda (&key data &allow-other-keys) (axe-util--log data) (message "Successfully deleted bucket %s" bucket)))
   bucket))

(provide 'axe-s3)
;;; axe-s3.el ends here
