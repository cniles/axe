;;; axe.el --- AWS Extensions -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Craig Niles

;; Author: Craig Niles <niles.c at gmail.com>
;; Maintainer: Craig Niles <niles.c at gmail.com>
;; URL: https://github.com/cniles/axe
;; Package-Requires: ((emacs "25.1") (hmac "0.0") (request "0.3.2") (s "1.12.0") (xmlgen "0.5") (dash "2.17.0") (mimetypes "1.0"))
;; Version: 0.0.1

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

;; AWS eXtensions for Emacs.  axe is an interface for interacting with
;; AWS resources.  axe aspires to provide an Emacs-based frontend as
;; an alternative to the AWS web console.  axe also tries to provide
;; more automated workflows for many tasks that are tediously typed
;; out through the AWS CLI utility.  It provides a library for
;; implementing functions that make AWS API requests and a buffer mode
;; to easily display paged API responses in Emacs buffers.

;;; Code:

(defvar axe-regions
  '((us-east-2 . "US East (Ohio)")
    (us-east-1 . "US East (N. Virginia)")
    (us-west-1 . "US West (N. California")
    (us-west-2 . "US West (Oregon)")
    (af-south-1 . "Africa (Cape Town)")
    (ap-east-1 . "Asia Pacific (Hong Kong)")
    (ap-south-1 . "Asia Pacific (Mumbai)")
    (ap-northeast-3 . "Asia Pacific (Osaka-Local)")
    (ap-northeast-2 . "Asia Pacific (Seoul)")
    (ap-southeast-1 . "Asia Pacific (Singapore)")
    (ap-southeast-2 . "Asia Pacific (Sydney)")
    (ap-northeast-1 . "Asia Pacific (Tokyo)")
    (ca-central-1 . "Canada (Central)")
    (cn-north-1 . "China (Beijing)")
    (cn-northwest-1 . "China (Ningxia)")
    (eu-central-1 . "Europe (Frankfurt)")
    (eu-west-1 . "Europe (Ireland)")
    (eu-west-2 . "Europe (London)")
    (eu-south-1 . "Europe (Milan)")
    (eu-west-3 . "Europe (Paris)")
    (eu-north-1 . "Europe (Stockholm)")
    (me-south-1 . "Middle East (Bahrain)")
    (sa-east-1 . "South America (São Paulo)"))
  "Association list mapping region code to region name.")

(defvar axe-region 'us-east-1
  "Region to use for API calls.")

(defvar axe-profile 'default
  "The AWS profile to use for reading credentials from file.")

(defvar axe-access-key-id nil
  "AWS Access key id.")

(defvar axe-secret-access-key nil
  "AWS Secret Access key id.")

(defvar axe-aws-credential-file
  (let ((home (getenv (cond ((eql system-type 'windows-nt) "USERPROFILE")
			    (t "HOME")))))
    (expand-file-name "credentials" (expand-file-name ".aws" home)))
  "Path to the default AWS credentials file.")

(provide 'axe)
;;; axe.el ends here
