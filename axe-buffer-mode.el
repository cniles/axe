;;; axe-buffer-mode.el --- Buffer mode for displaying AWS API responses -*- lexical-binding: t; package-lint-main-file: "axe.el"; -*-

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

;; Defines variables and functions to provide easy and uniform
;; integration of AWS APIs responses into the AXE Emacs buffer mode.
;; Chiefly, it provides a pattern for handling paged AWS API
;; responses.  It makes concessions for automatically extracting the
;; response next token from varying response content types, e.g. XML
;; or JSON and re-invoking the API with the next token.

;;; Code:

(require 'axe-util)

(defvar axe-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'axe-buffer--follow-next)
    (define-key map (kbd "N") 'axe-buffer--auto-follow)
    (define-key map (kbd "s") 'axe-buffer--stop-auto-follow)
    map)
  "AWS Interactive Buffer key map.")

(define-derived-mode axe-buffer-mode special-mode "AWS Interactive Buffer"
  "AWS Interactive Buffer")

(defvar axe-api-response-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'axe-buffer--follow-next)
    (define-key map (kbd "N") 'axe-buffer--auto-follow)
    (define-key map (kbd "s") 'axe-buffer--stop-auto-follow)
    map)
  "AWS API Response List Buffer key map.")

(define-derived-mode axe-api-response-list-mode tabulated-list-mode "AWS Interactive Buffer"
  "AWS Interactive Buffer")

(defvar-local axe-buffer-next-fn nil
  "Function to invoke to get the next AWS API response for this buffer.
Will be nil if AXE-BUFFER-AUTO-FOLLOW is enabled.")

(defvar-local axe-buffer-next-timer nil
  "Timer object for a pending invocation of next-fn.")

(defvar-local axe-buffer-auto-follow nil
  "Variable controlling whether to automatically request the next AWS API response.")

(defvar-local axe-buffer-auto-follow-delay nil
  "Variable setting the delay (in seconds) between AWS API requests when auto-following.")

(defun axe-buffer--follow-next ()
  "Call `axe-buffer-next-fn' for current buffer."
  (interactive)
  (if (not (functionp axe-buffer-next-fn))
      (message "Cannot follow without next function")
    (let ((func axe-buffer-next-fn))
      (setq axe-buffer-next-fn ())
      (funcall func))))
 
(cl-defun axe-buffer--auto-follow (&optional (delay 5.0))
  "Enable auto-follow in the current buffer.
The next request is made after DELAY seconds.  If not specified
the default is 5 seconds."
  (interactive)
  (if (not (functionp axe-buffer-next-fn))
      (message "Cannot auto follow without a next function.")
    (progn
      (setq-local axe-buffer-auto-follow t)
      (setq-local axe-buffer-auto-follow-delay delay)
      (funcall axe-buffer-next-fn))))

(cl-defun axe-buffer--stop-auto-follow ()
  "Disable auto-follow in the current buffer."
  (interactive)
  (if (null axe-buffer-auto-follow)
      (message "Auto-follow already disabled for buffer.")
    (setq-local axe-buffer-auto-follow nil)))

(cl-defun axe-buffer--make-next-handler (success api-fn params next-token-fn)
  "Make a lambda that handles paged API responses.

Wraps SUCCESS in a function that subsequently invokes an AWS API
function if a next token is provided.  If the API response it
receives contains a next token it will make another next handler
and apply API-FN to the values provided in PARAMS with the
received next token.  The next token value is read from the
response using the delegate NEXT-TOKEN-FN.

API-FN args must follow the following form:

    (success [required args] &key next-token [optional key args])

It is up to the caller to ensure required arguments are provided
in PARAMS."
  (cl-function
   (lambda (&key data response &allow-other-keys)
     (let ((next-token (if (functionp next-token-fn) (funcall next-token-fn :data data :response response))))
       (funcall
	success :data data :response response
	:next-fn (if (not (null next-token))
		     (lambda ()
		       (apply api-fn
			      (axe-buffer--make-next-handler success api-fn params next-token-fn)
			      (append params (list :next-token next-token))))))))))

(cl-defun axe-buffer-list (api-fn buffer-name list-fn api-fn-args keybindings-fn insert-fn next-token-fn &key auto-follow (auto-follow-delay 5.0))
  "Displays items returned by consecutive responses from API-FN.

Contents are displayed in the buffer BUFFER-NAME.  The contents
of the buffer will be erased and its minor mode changed.  Each
response should be unmarshallable into a list that is returned by
LIST-FN.  Each LIST-FN result is mapped with INSERT-FN.
INSERT-FN is expected to perform some insert option into the
current buffer (the output buffer created by the function).
Custom key bindings can be added to the output buffer using a
provided KEYBINDINGS-FN that takes a single key map as its
parameter.  Each call to API-FN is made using a next token (if
available) and all arguments in the API-FN-ARGS list.

Consecutive calls to API-FN require some form of a 'next token'
from the API.  NEXT-TOKEN-FN is called with the API response in
`make-function-handler` to retrieve the value.

Tail-calling of the next API response can be controlled with the
AUTO-FOLLOW and AUTO-FOLLOW-DELAY key parameters"
  (let ((inhibit-read-only t)
	(output-buffer (get-buffer-create buffer-name)))
    (with-current-buffer output-buffer
      (axe-buffer-mode)
      (setq axe-buffer-next-fn nil)
      (setq axe-buffer-next-timer nil)
      (setq axe-buffer-auto-follow auto-follow)
      (setq axe-buffer-auto-follow-delay auto-follow-delay)
      (if (functionp keybindings-fn)
	  (let ((map (make-sparse-keymap)))
	    (set-keymap-parent map axe-buffer-mode-map)
	    (use-local-map (funcall keybindings-fn map))))
      (erase-buffer)
      (setq buffer-read-only t))
    (apply
     api-fn
     (axe-buffer--make-next-handler
      (cl-function
       (lambda (&key response data next-fn &allow-other-keys)
	 (with-current-buffer output-buffer
	   (setq axe-buffer-next-timer nil)
	   (goto-char (point-max))
	   (let ((window (or (get-buffer-window output-buffer 'visible) (display-buffer output-buffer))))
	     (mapc (lambda (row)
		     (funcall insert-fn row :window window :response response))
		   (funcall list-fn :data data :response response)))
	   (if axe-buffer-auto-follow
	       (progn
		 (setq axe-buffer-next-fn nil)
		 (setq axe-buffer-next-timer
		       (run-at-time (format "%s sec" axe-buffer-auto-follow-delay) nil next-fn)))
	     (progn
	       (setq axe-buffer-next-fn next-fn))))))
      api-fn
      api-fn-args
      next-token-fn)
     api-fn-args)))

(cl-defun axe-list-api-results (api-fn buffer-name list-fn list-format api-fn-args keybindings-fn next-token-fn &key auto-follow (auto-follow-delay 5.0))
  "TODO"
  (let ((inhibit-read-only t)
	(output-buffer (get-buffer-create buffer-name)))
    (with-current-buffer output-buffer
      (axe-api-response-list-mode)
      (setq axe-buffer-next-fn nil)
      (setq axe-buffer-next-timer nil)
      (setq axe-buffer-auto-follow auto-follow)
      (setq axe-buffer-auto-follow-delay auto-follow-delay)
      (if (functionp keybindings-fn)
	  (let ((map (make-sparse-keymap)))
	    (set-keymap-parent map axe-api-response-list-mode-map)
	    (use-local-map (funcall keybindings-fn map))))
      (setq tabulated-list-format list-format)
      (setq tabulated-list-entries nil)
      (tabulated-list-init-header))
    (apply
     api-fn
     (axe-buffer--make-next-handler
      (cl-function
       (lambda (&key response data next-fn &allow-other-keys)
	 (with-current-buffer output-buffer
	   (setq axe-buffer-next-timer nil)
	   ;; This should follow Zen of Buffer Display.
	   (or (get-buffer-window output-buffer 'visible) (display-buffer output-buffer))
	   (let ((new-items (funcall list-fn :data data :response response)))
	     (if (null tabulated-list-entries)
		 (setq tabulated-list-entries new-items)
	       (nconc tabulated-list-entries (funcall list-fn :data data :response response))))
	   (tabulated-list-print)
	   (if axe-buffer-auto-follow
	       (progn
		 (setq axe-buffer-next-fn nil)
		 (setq axe-buffer-next-timer
		       (run-at-time (format "%s sec" axe-buffer-auto-follow-delay) nil next-fn)))
	     (progn
	       (setq axe-buffer-next-fn next-fn))))))
      api-fn
      api-fn-args
      next-token-fn)
     api-fn-args)))

(provide 'axe-buffer-mode)
;;; axe-buffer-mode.el ends here
