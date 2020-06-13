;;; aws-s3 -- S3 API and interactions -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'aws-api)
(require 'aws-util)
(require 'aws-buffer-mode)
(require 'xml)

(defun aws-s3--list-buckets (success)
  "Get buckets in the account and call SUCCESS when done."
  (aws-api-request
   "s3.amazonaws.com" 's3
   (transformed-invoker success 'parse-xml-string)
   "GET"
   :headers (list (cons "x-amz-content-sha256" (aws-api-sigv4-hash "")))))

(cl-defun aws-s3--list-objects-v2 (success bucket &key next-token delimiter encoding-type max-keys prefix start-after)
  "Call SUCCESS with result of List Object V2 on BUCKET.
Optional parameters NEXT-TOKEN, DELIMITER, ENCODING-TYPE,
MAX-KEYS, PREFIX and START-AFTER can also be specifiied.
NEXT-TOKEN corresponds to the `continuation-token` request
parameter."
  (aws-api-request
   (format "%s.s3.amazonaws.com" bucket) 's3
   (transformed-invoker success 'parse-xml-string)
   "GET"
   :query-params (rassq-edlete-all nil `(("list-type" . "2")
					 ("continuation-token" . ,next-token)))
   :headers (list (cons "x-amz-content-sha256" (aws-api-sigv4-hash "")))))

(defun aws-s3--insert-bucket (bucket)
  "Insert the details of BUCKET into the current buffer."
  (let ((inhibit-read-only t))
    (insert (propertize (format "%-30s" (alist-get 'CreationDate bucket)) 'bucket bucket 'font-lock-face 'shadow))
    (insert (propertize (format "%s" (alist-get 'Name bucket)) 'bucket bucket 'font-lock-face 'underline))
    (newline)))

(defun aws-s3--insert-content (content)
  "Insert the details of bucket content CONTENT into the current buffer."
  (let ((inhibit-read-only t))
    (insert (propertize (format "%-8s" (file-size-human-readable (string-to-number (alist-get 'Size content))))
			'font-lock-face 'shadow 'content content))
    (insert (propertize (format "%s" (alist-get 'Key content))
			'font-lock-face 'underline 'content content))
    (newline)))

(defun aws-s3-list-buckets ()
  "Lists buckets for the account into a buffer."
  (interactive)
  (aws-buffer-list
   'aws-s3--list-buckets
   "*aws-s3-bucket-list*"
   (lambda (response) (mapcar 'xml-node-to-alist (search-xml-children 'Bucket response)))
   ()
   (lambda (map)
     (define-key map (kbd "l") 'aws-s3-list-objects-in-bucket-at-point)
     map)
   'aws-s3--insert-bucket
   ()))

(defun aws-s3-list-objects-in-bucket-at-point ()
    "List contents of bucket described at from text property or text."
    (interactive)
    (let ((bucket (thing-or-property-at-point 'bucket 'Name)))
      (aws-s3-list-objects-v2 bucket)))

(defun aws-s3-list-objects-v2 (bucket)
  "List objects in BUCKET into a buffer."
  (interactive "sBucket: ")
  (aws-buffer-list
   'aws-s3--list-objects-v2
   (format "*aws-s3-object-list:%s*" bucket)
   (lambda (response)
     (mapcar 'xml-node-to-alist (search-xml-children 'Contents response)))
   (list bucket)
   ()
   'aws-s3--insert-content
   (lambda (response)
     (let ((next-token (alist-get 'NextContinuationToken (car (mapcar 'xml-node-to-alist (search-xml-children 'ListBucketResult response))))))
       (aws-sdk-log next-token)
       next-token))))

(provide 'aws-s3)
;;; aws-s3.el ends here
