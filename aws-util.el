;;; aws-util  --- Utility functions -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'json)

(defun now-millis ()
  "Return the current time in milliseconds since Jan 1, 1970 00:00:00 UTC."
  (truncate (* 1000 (time-to-seconds (current-time)))))

(defun make-signal-fn ()
  "Create a signal function that initially will return nil.

The signal function takes an optional argument NEW-VAL.  If
non-nil, the signal functions value to t and return the new
value."
  (let ((sig nil))
    (lambda (&optional new-val)
      (if (not (null new-val))
	  (setq sig new-val))
      sig)))

(defun trim-and-encode-alist (alst)
  "Return ALST JSON encoded with null keys removed."
  (json-encode-alist
   (rassq-delete-all
    nil alst)))

(defun format-with-alist (string alst &rest symbols)
  "Like `format` but derive placeholder values in STRING from ALST using SYMBOLS."
  (apply 'format string (mapcar (lambda (sym) (alist-get sym alst)) symbols)))

(defun xml-node-to-alist (node)
  "Return a list from NODE where all its children are alist conss'."
  (mapcar (lambda (child) (cons (car child) (car (last child)))) (nthcdr 2 node)))

(defun search-xml-children (sym children)
  "Search an XML parse tree CHILDREN for all nodes from tag name SYM."
  (if (null children) ()
    (mapcan
     (lambda (child)
       (if (listp child)
	   (if (eq sym (car child))
	       (list child)
	     (search-xml-children sym (nthcdr 2 child)))
	 ()))
     children)))

(defun parse-xml-string (string)
  "Parse and return tree represented by XML in STRING."
  (with-temp-buffer
    (insert string)
    (xml-parse-region)))

(defun transformed-invoker (fn transform-fn)
  "Create a lambda that transforms its argument with TRANSFORM-FN and invokes FN with the result."
  (lambda (arg) (funcall fn (funcall transform-fn arg))))

(cl-defun make-next-handler (success api-fn params next-token-fn)
  "Make a lambda that handles paged log group responses.

Wraps SUCCESS in a function that subsequently invokes an AWS API
function if a next token is provided.  If the log groups response
it receives contains a next token it will make another next
handler and apply API-FN to the values provided in PARAMS with
the received next token.  The next token value is read from the
API response using the symbol defined by TOKEN-PROP.

API-FN args must follow the following form:

    (success [required args] &key next-token [optional key args])

It is up to the caller to ensure REQUIRED ARGS are provided in
PARAMS."
  (lambda (response)
    (let ((next-token (if (functionp next-token-fn) (funcall next-token-fn response))))
      (funcall
       success response
       (lambda ()
	 (if (not (null next-token))
	     (apply api-fn
		    (make-next-handler success api-fn params next-token-fn)
		    (append params (list :next-token next-token)))
	   nil))))))

(defun thing-or-property-at-point (prop key)
  "Get the value for KEY of text property PROP or symbol at point."
  (let ((thing (get-text-property (point) prop)))
    (if (null thing) (thing-at-point 'symbol) (alist-get key thing))))

(defun aws-sdk-log(s)
  "Write string S to the AWS SDK log buffer."
  (with-current-buffer (get-buffer-create "*aws-sdk-logs*")
    (goto-char (point-max))
    (insert (format "%s\n\n" s))))

(provide 'aws-util)
;;; aws-util.el ends here
