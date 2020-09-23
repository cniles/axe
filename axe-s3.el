;;; axe-s3 -- S3 API and interactions -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'axe-api)
(require 'axe-util)
(require 'axe-buffer-mode)
(require 'xml)
(require 'xmlgen)

(defun axe--s3-list-buckets (success)
  "Get buckets in the account and call SUCCESS when done."
  (axe--api-request
   "s3.amazonaws.com" 's3
   success
   "GET"
   :parser 'xml-read
   :headers (list (cons "x-amz-content-sha256" (axe--api-sigv4-hash "")))))

(cl-defun axe--s3-list-objects-v2 (success bucket &key next-token delimiter encoding-type fetch-owner max-keys prefix start-after request-payer)
  "Call SUCCESS with result of List Object V2 on BUCKET.
Optional parameters NEXT-TOKEN, DELIMITER, ENCODING-TYPE,
MAX-KEYS, PREFIX and START-AFTER can also be specifiied.
NEXT-TOKEN corresponds to the `continuation-token` request
parameter."
  (axe--api-request
   (format "%s.s3.amazonaws.com" bucket) 's3
   success
   "GET"
   :parser 'xml-read
   :query-params (rassq-delete-all nil `(("list-type" . "2")
					 ("continuation-token" . ,next-token)
					 ("delimiter" . ,delimiter)
					 ("encoding-type" . ,encoding-type)
					 ("fetch-owner" . ,fetch-owner)
					 ("max-keys" . ,max-keys)
					 ("prefix" . ,prefix)
					 ("start-after" . ,start-after)))
   :headers (rassq-delete-all nil `(("x-amz-content-sha256" . ,(axe--api-sigv4-hash ""))
				    ("x-amz-request-payer" . ,request-payer)))))

(cl-defun axe--s3-create-bucket (success bucket &key acl grant-full-control grant-read grant-read-acp grant-write grant-write-acp object-lock-enabled-for-bucket location-constraint)
  "Create bucket with name BUCKET.
ACL, GRANT-FULL-CONTROL GRANT-READ GRANT-READ-ACP GRANT-WRITE
GRANT-WRITE-ACP OBJECT-LOCK-ENABLED-FOR-BUCKET and
LOCATION-CONSTRAINT parameters behave as described in API
CreateBucket documentation. See
`https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateBucket.html'"
  (let ((payload (if location-constraint
		     (xmlgen `(CreateBucketConfiguration :xmlns "http://s3.amazonaws.com/doc/2006-03-01/"
							 (LocationConstraint location-constraint)))
		   "")))
    (axe--api-request
     (format "%s.s3.amazonaws.com" bucket)
     's3
     success
     "PUT"
     :parser 'xml-read
     :headers (rassq-delete-all nil `(("Content-Type" . "text/plain")
				      ("x-amz-acl" . ,acl)
				      ("x-amz-grant-full-control" . ,grant-full-control)
				      ("x-amz-grant-read" . ,grant-read)
				      ("x-amz-grant-read-acp" . ,grant-read-acp)
				      ("x-amz-grant-write" . ,grant-write)
				      ("x-amz-grant-write-acp" . ,grant-write-acp)
				      ("x-amz-bucket-object-lock-enabled" . ,object-lock-enabled-for-bucket)
				      ("x-amz-content-sha256" . ,(axe--api-sigv4-hash payload))))
     :request-payload payload)))

(cl-defun axe--s3-delete-bucket (success bucket)
  "Create bucket with name BUCKET.
ACL, GRANT-FULL-CONTROL GRANT-READ GRANT-READ-ACP GRANT-WRITE
GRANT-WRITE-ACP OBJECT-LOCK-ENABLED-FOR-BUCKET and
LOCATION-CONSTRAINT parameters behave as described in API
CreateBucket documentation. See
`https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateBucket.html'"
  (axe--api-request
   (format "%s.s3.amazonaws.com" bucket)
   's3
   success
   "DELETE"
   :parser 'xml-read
   :headers (rassq-delete-all nil `(("x-amz-content-sha256" . ,(axe--api-sigv4-hash ""))))))

(cl-defun axe--s3-insert-bucket (bucket &key &allow-other-keys)
  "Insert the details of BUCKET into the current buffer."
  (let ((inhibit-read-only t))
    (insert (propertize (format "%-30s" (alist-get 'CreationDate bucket)) 'bucket bucket 'font-lock-face 'shadow))
    (insert (propertize (format "%s" (alist-get 'Name bucket)) 'bucket bucket 'font-lock-face 'underline))
    (newline)))

(cl-defun axe--s3-insert-content (content &key &allow-other-keys)
  "Insert the details of bucket content CONTENT into the current buffer."
  (let ((inhibit-read-only t))
    (insert (propertize (format "%-8s" (file-size-human-readable (string-to-number (alist-get 'Size content))))
			'font-lock-face 'shadow 'content content))
    (insert (propertize (format "%s" (alist-get 'Key content))
			'font-lock-face 'underline 'content content))
    (newline)))

;;;###autoload
(defun axe-s3-list-buckets ()
  "Lists buckets for the account into a buffer."
  (interactive)
  (axe--buffer-list
   'axe--s3-list-buckets
   "*axe-s3-bucket-list*"
   (cl-function (lambda (&key data &allow-other-keys) (mapcar 'xml-node-to-alist (search-xml-children 'Bucket data))))
   ()
   (lambda (map)
     (define-key map (kbd "l") 'axe-s3-list-objects-in-bucket-at-point)
     map)
   'axe--s3-insert-bucket
   ()))

;;;###autoload
(defun axe-s3-list-objects-in-bucket-at-point ()
    "List contents of bucket described at from text property or text."
    (interactive)
    (let ((bucket (thing-or-property-at-point 'bucket 'Name)))
      (axe-s3-list-objects-v2 bucket)))

;;;###autoload
(defun axe-s3-list-objects-v2 (bucket)
  "List objects in BUCKET into a buffer."
  (interactive "sBucket: ")
  (axe--buffer-list
   'axe--s3-list-objects-v2
   (format "*axe-s3-object-list:%s*" bucket)
   (cl-function
    (lambda (&key data &allow-other-keys)
      (mapcar 'xml-node-to-alist (search-xml-children 'Contents data))))
   (list bucket)
   ()
   'axe--s3-insert-content
   (cl-function
    (lambda (&key data &allow-other-keys)
      (let ((next-token (alist-get 'NextContinuationToken (xml-node-to-alist data))))
	next-token)))))

;;;###autoload
(defun axe-s3-create-bucket (bucket)
  "Create BUCKET."
  (interactive "sBucket: ")
  (axe--s3-create-bucket
   (cl-function (lambda (&key &allow-other-keys) (message (format "Successfully created bucket %s" bucket))))
   bucket))

;;;###autoload
(defun axe-s3-delete-bucket (bucket)
  "Delete BUCKET."
  (interactive "sBucket: ")
  (axe--s3-delete-bucket
   (cl-function (lambda (&key data &allow-other-keys) (axe-log data) (message (format "Successfully deleted bucket %s" bucket))))
   bucket))

(provide 'axe-s3)
;;; axe-s3.el ends here
