;;; pastery.el --- paste snippets to pastery.net. -*- lexical-binding: t; -*-

;; Author: Bruno Dias <dias.h.bruno@gmail.com>
;; Version: 0.2.1
;; Package-Requires: ((emacs "24.4") (request "0.2.0"))
;; Keywords: tools
;; Homepage: https://github.com/diasbruno/pastery.el

;;; Commentary:

;; This is the main file of the project.
;; It contains all autoloads and information
;; about the package.

;; See license.md.

;; This file is NOT part of GNU Emacs.

;;; Code:

(require 'cl-lib)
(require 'request)
(require 'subr-x)
(require 'pastery-data)
(require 'pastery-buffer)
(require 'pastery-net)
(require 'pastery-list-mode)

(defgroup pastery nil
  "Publish to pastery.net."
  :group 'application)

(defconst pastery-version "0.2.1"
  "Pastery for Emacs version.")

(defvar pastery-url "https://www.pastery.net/api/paste/"
  "Pastery url.")

(defvar pastery-emacs-user-agent "Mozilla/5.0 (Emacs) pastery"
  "User-agent saying that we are in Emacs.")

(defvar pastery-response-parser 'json-read
  "What to use to parse the response.")

(defcustom pastery-api-key ""
  "User's api key.  Prefer adding to your ~/.emacs file."
  :type 'string
  :group 'pastery)

(defcustom pastery-default-duration "1440"
  "Default duration."
  :type 'string
  :group 'pastery)

;;;###autoload
(defun pastery-submit ()
  "Create a pastery from a region."
  (interactive)
  (let* ((request-data (pastery--get-content))
         (from-buffer (current-buffer))
         (user-info (pastery--get-info-for-new-paste))
         (paste-info (pastery-net--build-query user-info))
         (request-url (concat pastery-url
                              "?api_key=" pastery-api-key "&"
                              paste-info)))
    (pastery-net--submit-paste
     request-url request-data
     (lambda (&rest args &key error-thrown &allow-other-keys)
       (print error-thrown))
     (lambda (&key data &allow-other-keys)
       (pastery--open-paste-buffer (cdr (assoc 'id data)) data)))
    (set-buffer from-buffer)
    t))

;;;###autoload
(defun pastery ()
  "List all my pastes."
  (interactive)
  (pastery-net--get-list-of-my-pastes
   (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                  (print error-thrown)))
   (cl-function (lambda (&key data &allow-other-keys)
                  (let* ((entries (cdar data)))
                    (if (eq (length entries) 0)
                        (message "No pastes.")
                      (pastery--make-tabular-list "*pastery-list*" entries)))))))

(provide 'pastery)
;;; pastery.el ends here
