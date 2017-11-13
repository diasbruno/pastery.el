;;; pastery-data.el --- paste snippets to pastery.net. -*- lexical-binding: t; -*-

;; Author: Bruno Dias <dias.h.bruno@gmail.com>
;; Version: 0.2.1
;; Package-Requires: ((emacs "24.4") (request "0.2.0"))
;; Keywords: tools
;; Homepage: https://github.com/diasbruno/pastery.el

;;; Commentary:

;; This file contains all the function
;; to transform data from elisp to pastery
;; and vice-versa.

;; See license.md.

;; This file is NOT part of GNU Emacs.

;;; Code:


(defun pastery--check-content (content)
  "Print to buffer empty message if the CONTENT is empty."
  (if (string= content "")
      "No 'pastes' available."
    content))

(defun stringify (x)
  "Make a string out of X if we need."
  (let ((v (cdr x)))
    (if (numberp v)
        (number-to-string v)
      v)))

(defun pastery--ask (prefix default-value)
  "Prompt to a user to ask.  Show format as PREFIX and (DEFAULT-VALUE)."
  (concat prefix
          (if (not (null default-value))
              (concat "(" default-value ")")
            "")
          ":"))

(defun pastery--ask-value-or-default (title k default-value)
  "Prompt for the user with a message TITLE.   Return a pair with (K . value)
if valid.  Otherwise, return the (K . DEFAULT-VALUE)."
  (let* ((value (read-string (pastery--ask title default-value))))
   (if (not (string-empty-p value))
       `(,k . ,value)
     `(,k . ,default-value))))

(defun pastery--get-info-for-new-paste ()
  "Get information for the paste that is been created."
  (let* ((title (pastery--ask-value-or-default "Title"
                                               'title
                                               nil))
         (duration (pastery--ask-value-or-default "Duration"
                                                  'duration
                                                  pastery-default-duration))
         (lang (pastery--ask-value-or-default "Language"
                                              'language
                                              nil)))
    (cl-remove-if (lambda (x) (null (cdr x)))
                  (list title duration lang))))

(defun pastery--format-json-key-value (paste-item)
  "Format each PASTE-ITEM from json."
  (let ((key (car paste-item))
        (value (cdr paste-item)))
    (concat (symbol-name key)
            ": "
            (if (numberp value)
                (number-to-string value)
              value))))

(defun pastery--format-paste-header (paste)
  "Format the PASTE from json and build the header.
Removing the 'body."
  (mapconcat #'pastery--format-json-key-value
             (cl-remove-if (lambda (x) (eq (car x) 'body)) paste) "\n"))

(defun pastery--paste-to-tabular-entry (paste-item)
  "Format the PASTE-ITEM to a tabular data format."
  (let ((values (mapcar #'stringify paste-item)))
    `(,(car values) ,(vconcat values))))

(defun pastery--response-to-tabular-entries (data)
  "Transform the api DATA response into tabular entries."
  (mapcar 'pastery--paste-to-tabular-entry data))

(provide 'pastery-data)
;;; pastery-data.el ends here
