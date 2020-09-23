;;; axe-util  --- Utility functions -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'json)
(require 's)
(eval-when-compile (require 'cl))

(defun axe--match-profile-line (line)
  "Return profile name if LINE is a profile line."
  (nth 1 (s-match "^\s*\\[\s*\\([a-zA-Z0-9-_]+\\)\s*]\s*$" line)))

(defun axe--match-key-value-line (line)
  "Return cons of key value pair if LINE is a key/value line."
  (let ((match (cdr (s-match "^\s*\\([^[:blank:]]+\\)\s*=\s*\\([^[:blank:]]+\\)\s*$" line))))
    (cons (car match) (nth 1 match))))

(defun axe--parse-credential-file ()
  "Read the contentds of the AWS crednetial file.

Returns an alist of profile names mapped to their key and
secret."
  (let ((props ())
	(profile ())
	(res ()))
    (dolist (line (with-temp-buffer
		    (insert-file-contents axe-aws-credential-file)
		    (split-string (buffer-string) "\n" t)))
      (let ((profile-line (axe--match-profile-line line))
	    (key-value-line (axe--match-key-value-line line)))
	(cond (profile-line
	       (push (cons profile props) res)
	       (setq profile profile-line)
	       (setq props ()))
	      (key-value-line
	       (push key-value-line props)))))
    (if profile (push (cons profile props) res))
    res))

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

(defun xml-read ()
  "Parse and return tree represented by xml in current buffer."
  (libxml-parse-xml-region (point) (point-max)))

(defun transformed-invoker (fn transform-fn)
  "Create a lambda that transforms its argument with TRANSFORM-FN and invokes FN with the result."
  (lambda (body get-header) (funcall fn (funcall transform-fn body) get-header)))

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
  (cl-function
   (lambda (&key data response &allow-other-keys)
     (let ((next-token (if (functionp next-token-fn) (funcall next-token-fn :data data :response response))))
       (funcall
	success :data data :response response
	:next-fn (if (not (null next-token))
		     (lambda ()
		       (apply api-fn
			      (make-next-handler success api-fn params next-token-fn)
			      (append params (list :next-token next-token))))))))))

(defun thing-or-property-at-point (prop key)
  "Get the value for KEY of text property PROP or symbol at point."
  (let ((thing (get-text-property (point) prop)))
    (if (null thing) (thing-at-point 'symbol) (alist-get key thing))))

(defun axe-log (s)
  "Write string S to the axe log buffer."
  (with-current-buffer (get-buffer-create "*axe-logs*")
    (goto-char (point-max))
    (insert (format "%s\n\n" s))))

(provide 'axe-util)
;;; axe-util.el ends here
