;;; hmac.el --- Hash-based message authentication code

;;; Version: 1.0
;;; Author: Sean McAfee
;;; Url: https://github.com/grimnebulin/emacs-hmac
;;; Package-Requires: ((emacs "25.1"))

;; Copyright 2018 Sean McAfee

;; This file is part of hmac.

;; hmac is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; hmac is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with hmac.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;;
;;; This is a simple wrapper around secure-hash that computes a
;;; hash-based message authentication code.

(require 'cl-lib)
(require 'subr-x)

;;; Code:

(defconst hmac-algorithm-blocksizes
  '((md5 . 64)
    (sha1 . 64)
    (sha224 . 64)
    (sha256 . 64)
    (sha384 . 128)
    (sha512 . 128))
  "Mapping from HMAC algorithm to the algorithm's blocksize.")


(defun hmac (algorithm key message &optional binary)
  "Compute the HMAC for a given message, private key and algorithm.

ALGORITHM is a symbol naming one of the algorithms recognized by
`secure-hash'.  KEY is a the private key to use.  MESSAGE, a
string, is the message to hash.  If BINARY is non-nil, the hmac
will be returned as a binary string, otherwise as a hexadecimal
string."
  (if-let (blocksize (alist-get algorithm hmac-algorithm-blocksizes))
      (progn
        (when (> (length key) blocksize)
          (setq key (secure-hash algorithm key)))
        (when (< (length key) blocksize)
          (setq key (concat key (make-string (- blocksize (length key)) 0))))
        (let ((o-key-pad (cl-map 'string #'logxor key (make-string blocksize #x5c)))
              (i-key-pad (cl-map 'string #'logxor key (make-string blocksize #x36))))
          (secure-hash algorithm (concat o-key-pad (secure-hash algorithm (concat i-key-pad message) nil nil t)) nil nil binary)))
    (error "Unsupported hash algorithm %s" algorithm)))

(provide 'hmac)


;;; hmac.el ends here
