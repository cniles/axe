;;; axe-buffer-mode --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'axe-util)

(defvar axe-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'axe-buffer-follow-next)
    (define-key map (kbd "N") 'axe-buffer-auto-follow)
    (define-key map (kbd "s") 'axe-buffer-stop-auto-follow)
    map)
  "AWS Interactive Buffer key map.")

(define-derived-mode axe-buffer-mode special-mode "AWS Interactive Buffer"
  "AWS Interactive Buffer")

(defvar-local axe-buffer-next-fn nil
  "Function to invoke to get the next AWS API response for this buffer.
Will be nil if AXE-BUFFER-AUTO-FOLLOW is enabled.")

(defvar-local axe-buffer-next-timer nil
  "Timer object for a pending invocation of next-fn.")

(defvar-local axe-buffer-auto-follow nil
  "Variable controlling whether to automaticallty request the next AWS API resposne.")

(defvar-local axe-buffer-auto-follow-delay nil
  "Variable setting the delay (in seconds) between AWS API requests when auto-following.")

(defun axe-buffer-follow-next ()
  "Call axe-buffer-next-fn for current buffer."
  (interactive)
  (if (not (functionp axe-buffer-next-fn))
      (message "Cannot follow without next function")
    (let ((func axe-buffer-next-fn))
      (setq axe-buffer-next-fn ())
      (funcall func))))

(cl-defun axe-buffer-auto-follow (&optional (delay 5.0))
  "Enables auto-follow in the current buffer."
  (interactive)
  (if (not (functionp axe-buffer-next-fn))
      (message "Cannot auto follow without a next function.")
    (progn
      (setq-local axe-buffer-auto-follow t)
      (setq-local axe-buffer-auto-follow-delay delay)
      (funcall axe-buffer-next-fn))))

(cl-defun axe-buffer-stop-auto-follow ()
  "Disables auto-follow in the current buffer."
  (interactive)
  (if (null axe-buffer-auto-follow)
      (message "Auto-follow already disabled for buffer.")
    (setq-local axe-buffer-auto-follow nil)))

(cl-defun axe-buffer-list (api-fn buffer-name list-fn api-fn-args keybindings-fn insert-fn next-token-fn &key auto-follow (auto-follow-delay 5.0))
  "Displays items returned by consecutive responses from API-FN.

Contents are displayed in the buffer BUFFER-NAME.  The contents
of the buffer will be erased and its minor mode changed.  Each
response should be unmarshalable into a list that is returned by
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
  (interactive)
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
    (apply api-fn
     (make-next-handler
      (lambda (response next-fn)
	(with-current-buffer output-buffer
	  (setq axe-buffer-next-timer nil)
	  (goto-char (point-max))
	  (mapc insert-fn (funcall list-fn response))
	  (or (get-buffer-window output-buffer 'visible) (display-buffer output-buffer))
	  (if axe-buffer-auto-follow
	      (progn
		(setq axe-buffer-next-fn nil)
		(setq axe-buffer-next-timer
		      (run-at-time (format "%s sec" axe-buffer-auto-follow-delay) nil next-fn)))
	    (progn
	      (setq axe-buffer-next-fn next-fn)))))
      api-fn
      api-fn-args
      next-token-fn)
     api-fn-args)))

(provide 'axe-buffer-mode)
;;; axe-buffer-mode.el ends here