# aws-el
An Emacs Interface to AWS
# Introduction
aws-el lets you use Emacs as a frontend for interacting with AWS
resources.  A number of tasks like listing buckets, invoking lambdas
and tailing logs are available ergonomically from Emacs.

This project is in a very early form.  A lot of things can, and will,
change.
# Setup
## Variables
The variables `aws-access-key-id` and `aws-secret-access-key` must be
defined.
## Keybindings
Keybindings should be implemented by the user. There isn't currently a
recommended list.
# Usage
TODO
# Known issues
## SSL errors from periods in bucket names
This is a known service stemming from the bucket name forming an
additional sub-domain.  It will make cURL burp a certificate error. A
work-around needs to be identified.
