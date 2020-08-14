;;; axe-lambda -- Package for interacting with AWS Lambda
;;; Commentary:
;;; Code:
(require 'axe-api)
(require 'axe-buffer-mode)

(cl-defun axe-lambda--list-functions (success &key function-version next-token master-region max-items)
  "List functions in the account and call SUCCESS when complete.

FUNCTION-VERSION, MASTER-REGION and MAX-ITEMS all correspond to
AWS API request parameters. NEXT-TOKEN is the MARKER parameter."
  (axe-api-request
   (axe-api-endpoint 'lambda axe-region)
   'lambda
   (transformed-invoker success 'json-read-from-string)
   "GET"
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
   (axe-api-endpoint 'lambda axe-region)
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
  (axe-buffer-list
   'axe-lambda--list-functions
   "*axe-lambda-functions*"
   (lambda (response) (alist-get 'Functions response))
   (list :function-version function-version :master-region master-region :max-items max-items)
   ()
   'axe-lambda--insert-function
   (lambda (response) (alist-get 'NextMarker response))))

(cl-defun axe-lambda--insert-function (fun)
  "Insert the details of lambda function FUN into the current buffer."
  (let ((inhibit-read-only t))
    (insert (propertize (format "%-15s" (file-size-human-readable (alist-get 'CodeSize fun))) 'function fun 'font-lock-face 'shadow))
    (insert (propertize (format "%s" (alist-get 'FunctionName fun)) 'function fun 'font-lock-face 'underline))
    (newline)))

(cl-defun axe-lambda-invoke-with-buffer (function-name buffer-or-name)
  "Invoke AWS Lambda function with name FUNCTION-NAME.

The contents of BUFFER-OR-NAME are used as the payload."
  (interactive
   (list
    (read-from-minibuffer "Function Name: ")
    (read-from-minibuffer "Buffer: " (buffer-name))))
  (let ((input-buffer (get-buffer buffer-or-name)))
    (axe-buffer-list
     'axe-lambda--invoke
     (format "*%s:%s-response*" (buffer-name input-buffer) function-name)
     (lambda (response) (list response))
     (let ((payload (with-current-buffer input-buffer (buffer-string))))
       (list function-name payload :log-type "Tail"))
     ()
     (lambda (response)
       (let ((inhibit-read-only t))
	 (insert response)))
     ())))

(provide 'axe-lambda)
;;; axe-lambda.el ends here
