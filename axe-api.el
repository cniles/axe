;;; axe-api.el --- Library for signing & making AWS API HTTP requests -*- lexical-binding: t; package-lint-main-file: "axe.el"; -*-

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

;; Implementation of the AWS sigv4 request signing process, as
;; defined here: https://docs.aws.amazon.com/general/latest/gr/sigv4-create-canonical-request.html

;;; Code:

(require 'axe)
(require 'axe-util)
(require 'cl-lib)
(require 'request)
(require 'hmac)
(require 'subr-x)
(require 'seq)
(require 's)

;; AWS Signature V4 Implementation

;; Note: Signature Version 4 does not require that you use a
;; particular character encoding to encode the canonical
;; request. However, some AWS services might require a specific
;; encoding. For more information, consult the documentation for that
;; service.

;; Check the backend used by request and warn if its not using `curl'.
(if (not (eq 'curl request-backend))
    (display-warning :error "axe requires curl be available."))

(defun axe-api--sigv4-hash (data)
  "Hash and return hex-encoded result of string DATA."
  (secure-hash 'sha256 (or data "")))

(defun axe-api--double-uri-encode (s)
  "Return double URI encoded form of string S."
  (url-hexify-string (url-hexify-string s)))

(defun axe-api--sigv4-canonical-uri-from-path (path-segments)
  "Forms canonical uri from PATH-SEGMENTS."
  (let ((path (mapconcat #'axe-api--double-uri-encode (cons "" (append path-segments ())) "/")))
    (if (or (null path) (equal "" path)) "/" path)))

(defun axe-api--sort-query-params (query-params)
  "Sort and return QUERY-PARAMS.

Sort first by key value pairs then by key name.  If there are
keys that are the same they are sorted by respective value."
  (sort query-params (lambda (a b)
    (let ((ca (car a))
	  (cb (car b)))
      (if (string= ca cb) (string< (cdr a) (cdr b))
	(string< ca cb))))))

(defun axe-api--uri-encode-query-param (param)
  "Encode query parameter PARAM's key and value."
  (let ((key (car param))
	(value (cdr param)))
    (cons (url-hexify-string key) (url-hexify-string value))))

(defun axe-api--sigv4-canonical-query-from-query-params (query-params)
  "Return the canonical forms of QUERY-PARAMS from URI filename part (path + query)."
  (mapconcat
   (lambda (param) (format "%s=%s" (car param) (cdr param)))
   (axe-api--sort-query-params (mapcar #'axe-api--uri-encode-query-param query-params)) "&"))

(defun axe-api--header-value-trimall (header-value)
  "Perform trimall operation as described in the AWS sigv4 implementation guide.

Remove all leading and trailing whitespace from HEADER-VALUE and
replace \s+ with ' ' (single space)."
  (replace-regexp-in-string "[[:space:]]+" " " (string-trim header-value)))

(defun axe-api--header-name-normalize (header-name)
  "Normalize HEADER-NAME by downcasing and trimming leading and trailing whitespace."
  (string-trim (downcase header-name)))

(defun axe-api--header-values-from-header-name (header-name headers)
  "Return a list of all headers matching HEADER-NAME in HEADERS.

Header names must have been normalized before hand.  Header names
are compared using equal."
  (mapcar #'cdr (seq-filter (lambda (c) (equal (car c) header-name)) headers)))

(defun axe-api--normalize-headers (headers)
  "Perform normalization on header names and values in HEADERS."
  (mapcar (lambda (header-param)
	    (cons (axe-api--header-name-normalize (car header-param))
		  (axe-api--header-value-trimall (cdr header-param))))
	  headers))

(defun axe-api--header-values-by-name (headers)
  "Group header values by their name.

Header names and values are normalized and returned sorted by
header name character code.  HEADERS is an alist where the car
and cdr of each are a header name and value, respectively.  This
function transforms the alist into a list of lists.  Each list
element is a list whose car is the header name, and the cdr is a
list of all values from HEADERS that fall under that header
name."
  (let* ((normalized-headers (axe-api--normalize-headers headers))
	 (header-names (delete-dups (sort (mapcar #'car normalized-headers) 'string<))))
    (mapcar (lambda (header-name)
	      (cons
	       header-name (axe-api--header-values-from-header-name
			    header-name normalized-headers)))
	    header-names)))

(defun axe-api--sigv4-canonical-headers-from-headers (headers)
  "Return the canonical headers formed from HEADERS.

HEADERS must be an alist.  Each item in HEADERS represents a
header key and value a pair.  The car of each item is the header
name and the cdr its respective value."
  (mapconcat
   (lambda (header-value-list)
     (let ((header-name (car header-value-list))
	   (header-values (cdr header-value-list)))
       (format "%s:%s\n" header-name
	       (mapconcat #'identity header-values ","))))
   (axe-api--header-values-by-name headers) ""))

(defun axe-api--sigv4-signed-headers-from-headers (headers)
  "Return list of signed header names from HEADERS separated by ';'."
  (mapconcat #'car (axe-api--header-values-by-name headers) ";"))

(defun axe-api--sigv4-canonical-request (method-type path-segments query-params headers request-payload)
  "Create AWS API Canonical request from provided parameters.

METHOD-TYPE is the HTTP request type (e.g. GET, PUT, etc).
PATH-SEGMENTS is a list of path parts that would normally be
separated by forward slashes.  QUERY-PARAMS is an alist of all
query parameter values.  HEADERS is an alist of all header
values.  REQUEST-PAYLOAD should be a string containing the
request body."
  (mapconcat #'identity (list method-type
			     (axe-api--sigv4-canonical-uri-from-path path-segments)
			     (axe-api--sigv4-canonical-query-from-query-params query-params)
			     (axe-api--sigv4-canonical-headers-from-headers headers)
			     (axe-api--sigv4-signed-headers-from-headers headers)
			     (axe-api--sigv4-hash request-payload)) "\n"))

(defun axe-api--time-to-iso8601-string (time)
  "Format TIME to an ISO8601string."
  (format-time-string "%Y%m%dT%H%M%SZ" time "UTC0"))

(defun axe-api--time-to-yyyymmdd-format (time)
  "Format TIME to an a string in YYYYMMDD format."
  (format-time-string "%Y%m%d" time "UTC0"))

(defun axe-api--sigv4-credential-scope-value (time region-code service-code)
  "Create the scope value as described in Task 2 of sigv4 developer guide from TIME, REGION-CODE and SERVICE-CODE."
  (format "%s/%s/%s/aws4_request"
	  (axe-api--time-to-yyyymmdd-format time)
	  region-code
	  service-code))

(defun axe-api--sigv4-string-to-sign (algorithm request-date-time region-code service-code hashed-canonical-request)
  "Create a string to sign value as described in Task2 of sigv4 developer guide.

ALGORITHM is a symbol naming one of the algorithms recognized by
`secure-hash`.  REQUEST-DATE-TIME is a time object as returned by
`current-time`.  REGION-CODE is the AWS region
code (e.g. `us-east-``).  SERVICE-CODE is the API service code,
such as `logs` or `lambda`.  HASHED-CANONICAL-REQUEST is the
hashed hash canonical request as defined in the AWS API sigv4
implementation guide.  See axe-api--sigv4-canonical-request and
axe-api--sigv4-hash."
  (mapconcat #'identity (list algorithm
			     (axe-api--time-to-iso8601-string request-date-time)
			     (axe-api--sigv4-credential-scope-value request-date-time region-code service-code)
			     hashed-canonical-request) "\n"))

(defun axe-api--sigv4-make-signing-key (k-secret request-date-time region-code service-code)
  "Create a sigv4 signing key.

K-SECRET is the secret access key.  REQUEST-DATE-TIME is a
timestamp as returned by CURRENT-TIME.  REGION-CODE is an AWS API
region code (e.g. `us-east-1`.  SERVICE-CODE is an AWS API
service code, such as `logs` or `lambda`."
  (with-coding-priority '(iso-8859-1)
    (let* ((k-date (hmac 'sha256 (concat "AWS4" k-secret) (axe-api--time-to-yyyymmdd-format request-date-time) t))
	   (k-region (hmac 'sha256 k-date (symbol-name region-code) t))
	   (k-service (hmac 'sha256 k-region (symbol-name service-code) t))
	   (k-signing (hmac 'sha256 k-service "aws4_request" t)))
      k-signing)))

(defun axe-api--sigv4-make-authorization-header-value (algorithm access-key-id secret-access-key region-code service-code path-segments query-params headers request-payload request-date-time method-type)
  "Create a sigv4 authorization header value.

Utilizes ALGORITHM, ACCESS-KEY-ID, SECRET-ACCESS-KEY,
REGION-CODE, SERVICE-CODE, PATH-SEGMENTS, QUERY-PARAMS, HEADERS,
REQUEST-PAYLOAD, REQUEST-DATE-TIME and METHOD-TYPE to calculate
the full AWS API sigv4 authorization header.."
  (let* ((canonical-request (axe-api--sigv4-canonical-request method-type path-segments query-params headers request-payload))
	 (hashed-canonical-request (axe-api--sigv4-hash canonical-request))
	 (string-to-sign (axe-api--sigv4-string-to-sign algorithm request-date-time region-code service-code hashed-canonical-request))
	 (signing-key (axe-api--sigv4-make-signing-key
		       secret-access-key
		       request-date-time
		       region-code
		       service-code))
	 (signed-headers (axe-api--sigv4-signed-headers-from-headers headers))
	 (signature (with-coding-priority '(iso-8859-1) (hmac 'sha256 signing-key string-to-sign))))
    (format "%s Credential=%s/%s, SignedHeaders=%s, Signature=%s"
	    algorithm access-key-id
	    (axe-api--sigv4-credential-scope-value request-date-time region-code service-code)
	    signed-headers signature)))

;; Naive implementation of a function to create an API domain. This
;; function needs to make region-code optional and domain
;; configurable.
(defun axe-api-domain (service-code region-code)
  "Return a properly formed API endpoint from SERVICE-CODE and REGION-CODE."
  (format "%s.%s.amazonaws.com" service-code region-code))

(defun axe-api--match-profile-line (line)
  "Return profile name if LINE is a profile line."
  (nth 1 (s-match "^\s*\\[\s*\\([a-zA-Z0-9-_]+\\)\s*]\s*$" line)))

(defun axe-api--match-key-value-line (line)
  "Return cons of key value pair if LINE is a key/value line."
  (let ((match (cdr (s-match "^\s*\\([^[:blank:]]+\\)\s*=\s*\\([^[:blank:]]+\\)\s*$" line))))
    (cons (car match) (nth 1 match))))

(defun axe-api--parse-credential-file ()
  "Read the contents of the AWS credential file.

Returns an alist of profile names mapped to their key and
secret."
  (let ((props ())
	(profile ())
	(res ()))
    (dolist (line (with-temp-buffer
		    (insert-file-contents axe-aws-credential-file)
		    (split-string (buffer-string) "\n" t)))
      (let ((profile-line (axe-api--match-profile-line line))
	    (key-value-line (axe-api--match-key-value-line line)))
	(cond (profile-line
	       (push (cons profile props) res)
	       (setq profile profile-line)
	       (setq props ()))
	      (key-value-line
	       (push key-value-line props)))))
    (if profile (push (cons profile props) res))
    res))

(cl-defun axe-api--get-credentials ()
  "Get the AWS API access key id and secret access key."
  (cond ((getenv "AWS_ACCESS_KEY_ID")
	 (list `(access-key-id . ,(getenv "AWS_ACCESS_KEY_ID")) `(secret-access-key . ,(getenv "AWS_SECRET_ACCESS_KEY"))))
	(axe-access-key-id
	 (list `(access-key-id . ,axe-access-key-id) `(secret-access-key . ,axe-secret-access-key)))
	((file-exists-p axe-aws-credential-file)
	 (let ((profile-props (cdr (assoc-string axe-profile (axe-api--parse-credential-file)))))
	   (list `(access-key-id . ,(cdr (assoc-string "aws_access_key_id" profile-props t)))
		 `(secret-access-key . ,(cdr (assoc-string "aws_secret_access_key" profile-props t))))))))

(cl-defun axe-api-request (host service-code success method-type
				&key
				encoding
				(region-code axe-region)
				(query-params ())
				(path-segments (list ""))
				(algorithm "AWS4-HMAC-SHA256")
				request-payload
				(headers ())
				(parser 'buffer-string))
  "Make a signed AWS API sigv4 request.
HOST should be the domain of the AWS API, usually formed by a
call to `axe-api-domain'.  SERVICE-CODE is the AWS API service
code, e.g. lambda or s3.  METHOD-TYPE is the HTTP method to use
for the API request, such as GET or POST.  Upon a successful
response, the callback SUCCESS will be invoked."

    (let* ((creds (axe-api--get-credentials))
	 (request-date-time (current-time))
	 (amz-date-header (cons "X-Amz-Date" (axe-api--time-to-iso8601-string request-date-time)))
	 (content-length-header (cons "Content-Length" (int-to-string (if request-payload (length request-payload) 0))))
	 (host-header (cons "Host" host))
	 (headers (append (list content-length-header host-header amz-date-header) headers ()))
	 (endpoint (concat "https://" host (s-join "/" (cons "" path-segments))))
	 (authorization-header
	  (cons "Authorization"
		(axe-api--sigv4-make-authorization-header-value
		 algorithm
		 (alist-get 'access-key-id creds)
		 (alist-get 'secret-access-key creds)
		 region-code
		 service-code
		 path-segments
		 query-params
		 headers
		 request-payload
		 request-date-time
		 method-type))))
    (request
      endpoint
      :params query-params
      ;; Setting User-Agent and Accept to empty string keeps curl from including these headers
      ;; Including them will prompt an error as they wouldn't have been included in the
      ;; canonical request signing.
      :headers (append headers (list authorization-header '("User-Agent" . "") '("Accept" . "") '("Accept-Encoding" . "") '("Content-Type" . "")) ())
      :parser parser
      :type method-type
      :data request-payload
      :encoding (or encoding 'utf-8)
      :error (cl-function
	      (lambda (&rest args &key data error-thrown &allow-other-keys)
		(axe-util--log "Error thrown:")
		(axe-util--log error-thrown)
		(axe-util--log "Error data:")
		(axe-util--log data)))
      :success (cl-function
		(lambda (&key data response &allow-other-keys)
		  (axe-util--log "Successful API response.")
		  (funcall success :data data :response response))))))

(provide 'axe-api)
;;; axe-api.el ends here
