;;; axe-logs.el --- Functions for working with AWS logs -*- lexical-binding: t; package-lint-main-file: "axe.el"; -*-

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

;; axe-logs.el provides functions for viewing AWS CloudWatch Logs
;; log-groups, streams and stream events.

;;; Code:

(require 'json)
(require 'thingatpt)
(require 'axe-api)
(require 'axe-util)
(require 'axe-buffer-mode)

(cl-defun axe-logs--describe-log-groups (success &key next-token prefix limit)
  "Request log groups using PREFIX and NEXT-TOKEN.

Provide results to callback function SUCCESS on completion.  Both
can be specified nil to omit."
  (axe-api-request
   (axe-api-domain 'logs axe-region)
   'logs
   success
   "POST"
   :parser #'json-read
   :headers '(("Content-Type" . "application/x-amz-json-1.1")
	      ("X-Amz-Target" . "Logs_20140328.DescribeLogGroups"))
   :request-payload (axe-util--trim-and-encode-alist
		     (list (cons "limit" limit)
			   (cons "nextToken" next-token)
			   (cons "logGroupNamePrefix" prefix)))))

(cl-defun axe-logs--describe-log-streams (success log-group-name &key next-token descending limit log-stream-name-prefix order-by)
  "Describe log streams for LOG-GROUP-NAME and invoke SUCCESS with the result.
NEXT-TOKEN, DESCENDING, LIMIT, LOG-STREAM-NAME-PREFIX and
ORDER-BY correspond to AWS API parameters."
  (axe-api-request
   (axe-api-domain 'logs axe-region)
   'logs
   success
   "POST"
   :parser #'json-read
   :headers '(("Content-Type" . "application/x-amz-json-1.1")
	      ("X-Amz-Target" . "Logs_20140328.DescribeLogStreams"))
   :request-payload (axe-util--trim-and-encode-alist
		     (list (cons "logGroupName" log-group-name)
			   (cons "descending" descending)
			   (cons "limit" limit)
			   (cons "logStreamNamePrefix" log-stream-name-prefix)
			   (cons "nextToken" next-token)
			   (cons "orderBy" order-by)))))

(cl-defun axe-logs--get-log-events (success log-group-name log-stream-name &key next-token end-time limit start-from-head start-time)
  "Get log events for stream with name LOG-STREAM-NAME of group LOG-GROUP-NAME.
Callback SUCCESS is invoked with the result.  NEXT-TOKEN,
END-TIME, LIMIT, START-FROM-HEAD, and START-TIME correspond to
AWS API params."
  (axe-api-request
   (axe-api-domain 'logs axe-region)
   'logs
   success
   "POST"
   :parser #'json-read
   :headers '(("Content-Type" . "application/x-amz-json-1.1")
	      ("X-Amz-Target" . "Logs_20140328.GetLogEvents"))
   :request-payload (axe-util--trim-and-encode-alist
		     (list (cons "logGroupName" log-group-name)
			   (cons "logStreamName" log-stream-name)
			   (cons "nextToken" next-token)
			   (cons "endTime" end-time)
			   (cons "limit" limit)
			   (cons "startFromHead" start-from-head)
			   (cons "startTime" start-time)))))

;;;###autoload
(defun axe-logs-describe-log-groups (prefix)
  "Opens a new buffer and displays all log groups.
If PREFIX is not nil, it is used to filter by log group name
prefix.  May result in multiple API calls.  If that is the case
then subsequent results may take some time to load and
displayed."
  (interactive "sPrefix: ")
  (let ((prefix (if (equal "" prefix) nil prefix))
	(limit ()))
    (axe-list-api-results
     #'axe-logs--describe-log-groups
     "*axe-log-groups*"
     (cl-function
      (lambda (&key data &allow-other-keys)
	(mapcar
	 (lambda (log-group)
	   (list nil
		 (vector
		  (format-time-string "%F %T" (seconds-to-time (/ (alist-get 'creationTime log-group) 1000)))
		  (file-size-human-readable (alist-get 'storedBytes log-group))
		  (list (format "%s" (alist-get 'logGroupName log-group)) 'log-group log-group))))
	 (alist-get 'logGroups data))))
     [("Creation Time" 20 t) ("Size" 8 t) ("Log Group Name" 1 t)]
     (list :prefix prefix :limit limit)
     (lambda (map)
       (define-key map (kbd "l") #'axe-logs-latest-log-stream-near-point)
       map)
     (cl-function (lambda (&key data &allow-other-keys)
		    (alist-get 'nextToken data)))
     :auto-follow t
     :auto-follow-delay 0.1)))

;;;###autoload
(cl-defun axe-logs-get-log-events (log-group-name log-stream-name &key auto-follow (auto-follow-delay 5.0))
  "Display log events for stream with name LOG-STREAM-NAME in LOG-GROUP-NAME.
Specifying FOLLOW-NEXT as non-nil will start the buffer in follow
mode.  In follow mode the next API request will automatically be
executed after FOLLOW-DELAY seconds (default 5 seconds)."
  (interactive "sLog Group Name:
sLog Stream Name: ")
  (axe-list-api-results
   #'axe-logs--get-log-events
   (format "*axe-log-stream:%s*" log-stream-name)
   (cl-function
    (lambda (&key data &allow-other-keys)
      (mapcar
       (lambda (log-event)
	 (list nil
	       (vector
		(format-time-string "%F %T" (seconds-to-time (/ (alist-get 'timestamp log-event) 1000)))
		(list (format "%s" (s-trim (alist-get 'eventId log-event))) 'log-event log-event)
		(format "%s" (substring (json-encode-string (alist-get 'message log-event)) 1 -1)))))
       (alist-get 'events data))))
   [("Timestamp" 20 t) ("Event ID" 10 t) ("Message" 1 t)]
   (list log-group-name log-stream-name)
   ()
   (cl-function (lambda (&key data &allow-other-keys)
     (alist-get 'nextForwardToken data)))
   :auto-follow auto-follow
   :auto-follow-delay auto-follow-delay))

;;;###autoload
(defun axe-logs-latest-log-stream-near-point ()
  "Open the log stream defined near the current point.
First checks for text property log-group on the line at point
otherwise uses the text at point in the buffer."
  (interactive)
  (let ((log-group-name (axe-util--thing-or-property-near-point 'log-group 'logGroupName)))
    (axe-logs--describe-log-streams
     (cl-function
      (lambda (&key data &allow-other-keys)
	(let ((log-stream-name (alist-get 'logStreamName (elt (alist-get 'logStreams data) 0))))
	  (axe-logs-get-log-events log-group-name log-stream-name))))
     log-group-name
     :limit 1
     :descending t
     :order-by "LastEventTime")))

(provide 'axe-logs)
;;; axe-logs.el ends here
