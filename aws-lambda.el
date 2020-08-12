;;; aws-lambda -- Package for interacting with AWS Lambda
;;; Commentary:
;;; Code:
(require 'aws-api)
(require 'aws-buffer-mode)

(cl-defun aws-lambda--list-functions (success &key function-version next-token master-region max-items)
  "List functions in the account and call SUCCESS when complete.

FUNCTION-VERSION, MASTER-REGION and MAX-ITEMS all correspond to
AWS API request parameters. NEXT-TOKEN is the MARKER parameter."
  (aws-api-request
   (aws-api-endpoint 'lambda aws-region)
   'lambda
   (transformed-invoker success 'json-read-from-string)
   "GET"
   :path-segments '("2015-03-31" "functions")
   :query-params (rassq-delete-all nil `(("FunctionVersion" . ,function-version)
					 ("Marker" . ,next-token)
					 ("MasterRegion" . ,master-region)
					 ("MaxItems" . ,max-items)))))

(defun aws-lambda--invoke (success function-name payload &key client-context invocation-type log-type qualifier)
  "Invoke AWS Lambda function with name FUNCTION-NAME and PAYLOAD.

SUCCESS is invoked upon completion.  Parameters CLIENT-CONTEXT,
INVOCATION-TYPE, LOG-TYPE and QUALIFIER operate as described in
AWS API request parameters.

See `https://docs.aws.amazon.com/lambda/latest/dg/API_Invoke.html'."
  (aws-api-request
   (aws-api-endpoint 'lambda aws-region)
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
(cl-defun aws-lambda-list-functions (&key function-version master-region max-items)
  "List functions in the account.

Parameters FUNCTION-VERSION, MASTER-REGION and MAX-ITEMS
correspond to AWS API request parameters.

See `https://docs.aws.amazon.com/lambda/latest/dg/API_ListFunctions.html'."
  (interactive)
  (aws-buffer-list
   'aws-lambda--list-functions
   "*aws-lambda-functions*"
   (lambda (response) (alist-get 'Functions response))
   (list :function-version function-version :master-region master-region :max-items max-items)
   ()
   'aws-lambda--insert-function
   (lambda (response) (alist-get 'NextMarker response))))

(cl-defun aws-lambda--insert-function (fun)
  "Insert the details of lambda function FUN into the current buffer."
  (let ((inhibit-read-only t))
    (aws-sdk-log fun)
    (insert (propertize (format "%-15s" (file-size-human-readable (alist-get 'CodeSize fun))) 'function fun 'font-lock-face 'shadow))
    (insert (propertize (format "%s" (alist-get 'FunctionName fun)) 'function fun 'font-lock-face 'underline))
    (newline)))

(provide 'aws-lambda)
;;; aws-lambda.el ends here
