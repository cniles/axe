;;; axe-lambda.el --- Package for interacting with AWS Lambda -*- lexical-binding: t; package-lint-main-file: "axe.el"; -*-

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

;; axe-lambda.el provides functions for listing and invoking AWS
;; lambda functions.

;;; Code:

(require 'axe-api)
(require 'axe-buffer-mode)

(defvar-local axe-lambda-log-buffer nil
  "The log buffer associated with an AWS Lambda result window.")

(defun axe-lambda--delete-result-window ()
    "Close a lambda result window and its log window."
    (interactive)
    (delete-window (get-buffer-window axe-lambda-log-buffer))
    (delete-window))

(defvar axe-lambda-result-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'axe-lambda--delete-result-window)
    map))

(define-derived-mode axe-lambda-result-mode
  special-mode "Lambda Result"
  "Major mode for showing AWS Lambda result.")

(cl-defun axe-lambda--list-functions (success &key function-version next-token master-region max-items)
  "List functions in the account and call SUCCESS when complete.

FUNCTION-VERSION, MASTER-REGION and MAX-ITEMS all correspond to
AWS API request parameters.  NEXT-TOKEN is the MARKER parameter."
  (axe-api-request
   (axe-api-domain 'lambda axe-region)
   'lambda
   success
   "GET"
   :parser #'json-read
   :path-segments '("2015-03-31" "functions")
   :query-params (rassq-delete-all nil `(("FunctionVersion" . ,function-version)
					 ("Marker" . ,next-token)
					 ("MasterRegion" . ,master-region)
					 ("MaxItems" . ,max-items)))))

(cl-defun axe-lambda--invoke (success function-name payload &key client-context invocation-type log-type qualifier)
  "Invoke AWS Lambda function with name FUNCTION-NAME and PAYLOAD.

SUCCESS is invoked upon completion.  Parameters CLIENT-CONTEXT,
INVOCATION-TYPE, LOG-TYPE and QUALIFIER operate as described in
AWS API request parameters.

See `https://docs.aws.amazon.com/lambda/latest/dg/API_Invoke.html'."
  (axe-api-request
   (axe-api-domain 'lambda axe-region)
   'lambda
   success
   "POST"
   :path-segments `("2015-03-31" "functions" ,function-name "invocations")
   :query-params (rassq-delete-all nil `(("Qualifier" . , qualifier)))
   :headers (rassq-delete-all nil `(("X-Amz-Invocation-Type" . ,invocation-type)
				    ("X-Amz-Log-Type" . ,log-type)
				    ("X-Amz-Client-Context" . ,client-context)))
   :request-payload payload))

;;;###autoload
(cl-defun axe-lambda-list-functions (&key function-version master-region max-items)
  "List functions in the account.

Parameters FUNCTION-VERSION, MASTER-REGION and MAX-ITEMS
correspond to AWS API request parameters.

See `https://docs.aws.amazon.com/lambda/latest/dg/API_ListFunctions.html'."
  (interactive)
  (axe-list-api-results
   #'axe-lambda--list-functions
   "*axe-lambda-functions*"
   (cl-function
    (lambda (&key data &allow-other-keys)
      (mapcar
       (lambda (fun)
	 (list nil
	       (vector
		(format "%s" (file-size-human-readable (alist-get 'CodeSize fun)))
		(list (format "%s" (alist-get 'FunctionName fun)) 'function fun))))
       (alist-get 'Functions data))))
   [("Code Size" 10 t) ("Function Name" 1 t)]
   (list :function-version function-version :master-region master-region :max-items max-items)
   ()
   (cl-function (lambda (&key data &allow-other-keys) (alist-get 'NextMarker data)))))

;;;###autoload
(cl-defun axe-lambda-invoke-with-buffer (function-name buffer-or-name)
  "Invoke AWS Lambda function with name FUNCTION-NAME.
The contents of BUFFER-OR-NAME are used as the payload."
  (interactive
   (list
    (read-from-minibuffer "Function Name: ")
    (read-from-minibuffer "Buffer: " (buffer-name))))
  (let* ((input-buffer (get-buffer buffer-or-name))
	 (payload (with-current-buffer input-buffer (buffer-string))))
    (axe-lambda--invoke
     (cl-function
      (lambda (&key data response &allow-other-keys)
	(let ((output-buffer (get-buffer-create (format "*%s:%s-response*" (buffer-name input-buffer) function-name)))
	      (log-buffer (get-buffer-create (format "%s%s*" (buffer-name) "logs*"))))
	  (with-current-buffer-window
	      output-buffer '((display-buffer-reuse-window) (window-height . fit-frame-to-buffer)) nil
	    (insert data)
	    (axe-lambda-result-mode)
	    (setq axe-lambda-log-buffer log-buffer))
	  (with-selected-window (get-buffer-window output-buffer)
	    (with-current-buffer-window
		log-buffer '((display-buffer-below-selected) (window-height . 20)) nil
	      (insert (base64-decode-string (request-response-header response "X-Amz-Log-Result")))
	      (special-mode))))))
     function-name
     payload
     :log-type "Tail")))

(provide 'axe-lambda)
;;; axe-lambda.el ends here
