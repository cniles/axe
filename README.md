# axe

AWS eXtensions for Emacs

# Introduction
axe lets you use Emacs as a frontend for interacting with AWS
resources.  It makes available a number of tasks, such as listing
buckets, invoking lambdas and tailing logs, ergonomically available
through emacs.

The package relies on the AWS APIs, rather than the CLI.  Their
reasoning behind this design decision (rather than depending on the AWS CLI) are:

0. The AWS CLI utility is no longer a runtime dependency.
1. The AWS CLI utility v1 and v2 have version differences
2. The AWS APIs are platform independent.
3. AXE can more easily control when updating to a new API version
   rather than dealing with current and future breaking changes
   between AWS CLI versions
4. The AWS APIs provide a more consistent response structure to
   operate upon.

This project is in a very early form.  A lot of things may change.
Breaking changes to the interactive functions will be avoided.
However, implementing against some of the internal interfaces could
possibly cause some heartburn with future changes.

# Setup

The variables `aws-access-key-id` and `aws-secret-access-key` can be
defined to set access key id and secret access key.  If not specified,
these values will be read from the AWS credential file set to
`aws-credential-file`, which defaults to the
`<user-home>/.aws/credentials`.

A profile can be set using `axe-profile` (defaults to 'default').

# Keybindings

Keybindings to specific interactive should be implemented by the
user. There isn't currently a recommended list.

The `axe-buffer-mode` does provide a set of keybindings for dealing
with API responses.

0. `n`: Request and display the next API response using the next token
   from the latest response.
1. `N`: Enable auto-follow in the buffer.  If a next token is provided
   in an API response, the next response will be requested and
   displayed after a configured delay.
2. `s`: Stop auto-following.


# Known issues

## SSL errors from periods in bucket names

This is a known issue with the service stemming from the bucket name forming an
additional sub-domain.  It will make cURL burp a certificate error. A
work-around needs to be identified.
