;;; axe-s3 -- S3 API and interactions -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'axe-api)
(require 'axe-util)
(require 'axe-buffer-mode)
(require 'xml)

(defun axe-s3--list-buckets (success)
  "Get buckets in the account and call SUCCESS when done."
  (axe-api-request
   "s3.amazonaws.com" 's3
   (transformed-invoker success 'parse-xml-string)
   "GET"
   :headers (list (cons "x-amz-content-sha256" (axe-api-sigv4-hash "")))))

(cl-defun axe-s3--list-objects-v2 (success bucket &key next-token delimiter encoding-type max-keys prefix start-after)
  "Call SUCCESS with result of List Object V2 on BUCKET.
Optional parameters NEXT-TOKEN, DELIMITER, ENCODING-TYPE,
MAX-KEYS, PREFIX and START-AFTER can also be specifiied.
NEXT-TOKEN corresponds to the `continuation-token` request
parameter."
  (axe-api-request
   (format "%s.s3.amazonaws.com" bucket) 's3
   (transformed-invoker success 'parse-xml-string)
   "GET"
   :query-params (rassq-delete-all nil `(("list-type" . "2")
					 ("continuation-token" . ,next-token)))
   :headers (list (cons "x-amz-content-sha256" (axe-api-sigv4-hash "")))))

(defun axe-s3--insert-bucket (bucket)
  "Insert the details of BUCKET into the current buffer."
  (let ((inhibit-read-only t))
    (insert (propertize (format "%-30s" (alist-get 'CreationDate bucket)) 'bucket bucket 'font-lock-face 'shadow))
    (insert (propertize (format "%s" (alist-get 'Name bucket)) 'bucket bucket 'font-lock-face 'underline))
    (newline)))

(defun axe-s3--insert-content (content)
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
  (axe-buffer-list
   'axe-s3--list-buckets
   "*axe-s3-bucket-list*"
   (lambda (response) (mapcar 'xml-node-to-alist (search-xml-children 'Bucket response)))
   ()
   (lambda (map)
     (define-key map (kbd "l") 'axe-s3-list-objects-in-bucket-at-point)
     map)
   'axe-s3--insert-bucket
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
  (axe-buffer-list
   'axe-s3--list-objects-v2
   (format "*axe-s3-object-list:%s*" bucket)
   (lambda (response)
     (mapcar 'xml-node-to-alist (search-xml-children 'Contents response)))
   (list bucket)
   ()
   'axe-s3--insert-content
   (lambda (response)
     (let ((next-token (alist-get 'NextContinuationToken (car (mapcar 'xml-node-to-alist (search-xml-children 'ListBucketResult response))))))
       (axe-log next-token)
       next-token))))

(provide 'axe-s3)
;;; axe-s3.el ends here
