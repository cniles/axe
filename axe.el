;;; axe -- AWS Extensions for Emacs
;;; Commentary:
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

(defun axe-region-code-is-valid-p (region-code)
  "Check if REGION-CODE is valid.
To be valid it must exist in `axe-regions' as a key."
  (if (eq (assoc region-code axe-regions) nil) nil t))

(provide 'axe)
;;; axe.el ends here
