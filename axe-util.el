;;; axe-util.el --- Utility functions -*- lexical-binding: t; package-lint-main-file: "axe.el"; -*-

;; Copyright (C) 2020 Craig Niles

;; Author: Craig Niles <niles.c at gmail.com>
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

;; axe-util.el contains utility methods used throughout the axe
;; package.

;;; Code:

(require 'axe)
(require 'json)
(require 's)
(require 'cl-lib)

;; Check that libxml2 is available and display a warning if not.
(if (null (if (fboundp 'libxml-available-p)
	      (libxml-available-p)
	    (and (fboundp 'libxml-parse-xml-region)
		 (with-temp-buffer
		   (insert "<body></body>")
		   (libxml-parse-xml-region (point-min) (point-max))))))
    (display-warning :error "libxml2 not compiled with Emacs! Some functionality may be unavailable."))

(defun axe-util--now-millis ()
  "Return the current time in milliseconds since Jan 1, 1970 00:00:00 UTC."
  (truncate (* 1000 (time-to-seconds (current-time)))))

(defun axe-util--make-signal-fn ()
  "Create a signal function that initially will return nil.

The signal function takes an optional argument NEW-VAL.  If
non-nil, the signal functions value to t and return the new
value."
  (let ((sig nil))
    (lambda (&optional new-val)
      (if (not (null new-val))
	  (setq sig new-val))
      sig)))

(defun axe-util--trim-and-encode-alist (alst)
  "Return ALST JSON encoded with null keys removed."
  (json-encode-alist
   (rassq-delete-all
    nil alst)))

(defun axe-util--format-with-alist (string alst &rest symbols)
  "Like `format` but derive placeholder values in STRING from ALST using SYMBOLS."
  (apply #'format string (mapcar (lambda (sym) (alist-get sym alst)) symbols)))

(defun axe-util--xml-node-to-alist (node)
  "Return a list from NODE where all its children are alist conss'."
  (mapcar (lambda (child) (cons (car child) (car (last child)))) (nthcdr 2 node)))

(defun axe-util--search-xml-children (sym children)
  "Search for tags named SYM in XML parse tree CHILDREN."
  (if (null children) ()
    (apply #'append
	   (mapcar
	    (lambda (child)
	      (if (listp child)
		  (if (eq sym (car child))
		      (list child)
		    (axe-util--search-xml-children sym (nthcdr 2 child)))
		()))
	    children))))

(defun axe-util--xml-read ()
  "Parse and return tree represented by xml in current buffer."
  (libxml-parse-xml-region (point) (point-max)))

(defun axe-util--transformed-invoker (fn transform-fn)
  "Create a lambda that will call FN after tranforming args with TRANSFORM-FN."
  (lambda (body get-header) (funcall fn (funcall transform-fn body) get-header)))

(defun axe-util--search-line-for-property (prop)
  "Search for text property PROP on the at point."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (while (and (< (point) (line-end-position)) (not (get-text-property (point) prop)))
      (right-char))
    (get-text-property (point) prop)))

(defun axe-util--thing-or-property-near-point (prop key)
  "Return the text property on current line  or thing at point.
First, check for PROP in the text properties on line at point.
If found, return KEY from that property.  If not, return
`thing-at-point'."
  (let ((thing (axe-util--search-line-for-property prop)))
    (if (consp thing) (alist-get key thing) (thing-at-point 'symbol))))

(defun axe-util--log (s)
  "Write string S to the axe log buffer."
  (with-current-buffer (get-buffer-create "*axe-logs*")
    (goto-char (point-max))
    (insert (format "%s\n\n" s))))

(defun axe-util--hexify (str)
  "Hex-encode and return the string STR."
  (mapconcat (lambda (c) (format "%X" c)) str ""))

(provide 'axe-util)
;;; axe-util.el ends here
