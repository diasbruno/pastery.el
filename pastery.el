;;; pastery.el --- Emacs integration for pastery.net. -*- lexical-binding: t; -*-

;; Author: Bruno Dias <dias.h.bruno@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.1") (request "0.2.0")
;; Keywords: tools
;; Homepage: https://github.com/diasbruno/pastery.el

;; This file is NOT part of GNU Emacs.

(eval-when-compile
  (require 'cl))

(require 'request)

(defgroup pastery nil
  "Publish to pastery.net."
  :group 'application)

(defconst pastery-version "0.1.0"
  "Pastery for emacs version.")

(defvar pastery-url "https://www.pastery.net/api/paste/"
  "Pastery url.")

(defvar pastery-emacs-user-agent "Mozilla/5.0 (Emacs) pastery"
  "User-agent saying that we are in emacs.")

(defvar pastery-response-parser 'json-read
  "What to use to parse the response.")

(defcustom pastery-api-key ""
  "User's api key. Prefer adding to your ~/.emacs file."
  :type 'string
  :group 'pastery)

(defcustom pastery-default-duration "1440"
  "Default duration."
  :type 'string
  :group 'pastery)

(defun pastery--format-paste-item (paste-item)
  "Format each paste key/value from the paste json."
  (let ((key (car paste-item))
        (value (cdr paste-item)))
    (concat (symbol-name key)
            ": "
            (if (numberp value)
                (number-to-string value)
              value))))

(defun pastery--format-paste (paste)
  (mapconcat 'pastery--format-paste-item paste "\n"))

(defun pastery--format-list (pastes)
  "Format a SEQUENCE of pastes."
  (mapconcat 'identity
             (seq-map (lambda (x) (pastery--format-paste x)) pastes)
             "\n\n"))

(defun pastery--region-substring (beg end)
  "Get the region if available."
  (if (and beg end)
      (buffer-substring-no-properties beg end)
    ""))

(defun pastery--buffer ()
  "Get the content from the current buffer."
  (pastery--region-substring (point-min) (point-max)))

(defun pastery--get-content ()
  "Get the content of the pastery from buffer or region."
  (interactive)
  (if (region-active-p)
      (pastery--region-substring (point) (mark))
    (pastery--buffer)))

(defun pastery--query-item (key value)
  "Creates a query item only if available."
  (if value
      (concat (symbol-name key) "=" value)
    ""))

(defun pastery--ask (prefix default-value)
  "Prompts to a user to ask."
  (concat prefix "(" default-value ")" ":"))

(defun pastery--info ()
  (let* ((title (read-string "Title: "))
         (dur (read-string (pastery--ask "Duration" pastery-default-duration)))
         (lang (read-string "Language: "))
         (maxviews (read-string "Max views: "))
         (files (read-string "Files: "))
         ;; finish reading strings
         (paste-title (if (not (string-empty-p title))
                          `(title . ,title)
                        nil))
         (paste-duration (if (not (string-empty-p dur))
                             `(duration . ,dur)
                           `(duration . ,pastery-default-duration)))

         (paste-lang (if (not (string-empty-p lang))
                         `(language . lang)
                       nil))
         (paste-max-views (if (not (string-empty-p maxviews))
                              `(max-views . maxviews)
                            nil))
         (paste-files (if (not (string-empty-p files))
                          `(files . files)
                        nil))

         (paste (list paste-title
                      paste-duration
                      paste-lang
                      paste-max-views
                      paste-files)))
    (remove-if (lambda (x) (eq x nil)) paste)))

(defun pastery--request (method request-url request-data err succ)
  (request request-url
           :type method
           :parser 'json-read
           :data 'request-data
           :error err
           :success succ))

(defun pastery--place-buffer (paste-buffer-name data)
  "Place the data to a buffer."
  (when data
    (with-current-buffer (get-buffer-create paste-buffer-name)
      (erase-buffer)
      (insert (concat "Pastery list\n\n"
                      (pastery--format-list (cdar data))
                      "\n"))
      (pop-to-buffer (current-buffer)))))

(defun pastery/submit ()
  "Create a pastery from a region."
  (let* ((request-data (pastery--get-content))
         (from-buffer (current-buffer))
         (user-info (pastery--info))
         (paste-info (mapconcat
                      (lambda (x) (pastery--query-item (car x) (cdr x)))
                      user-info "&"))
         (request-url (concat pastery-url paste-info)))
    (pastery--request "POST" request-url request-data
                      (cl-function
                       (lambda (&rest args &key error-thrown &allow-other-keys)
                         (print error-thrown)))
                      (cl-function
                       (lambda (&key data &allow-other-keys)
                         (let* ((paste-id (cdr (assoc 'id data)))
                                (paste-buffer-name (concat "*pastery-" paste-id "*"))
                                (paste-data `((pastes . ,(vector data)))))
                           (pastery--place-buffer paste-buffer-name paste-data)))))
    (set-buffer from-buffer)
    t))

(defun pastery/list ()
  "List all my pastes."
  (interactive)
  (let ((request-url (concat pastery-url "?api_key=" pastery-api-key)))
    (pastery--request "GET" request-url nil
                      (cl-function
                       (lambda (&rest args &key error-thrown &allow-other-keys)
                         (print error-thrown)))
                      (cl-function
                       (lambda (&key data &allow-other-keys)
                         (pastery--place-buffer "*pastery-list*" data))))
    (message "Fetching list...")))

(defun pastery/get (paste-id)
  "Get a paste by id."
  (interactive)
  (let ((request-url (concat (concat pastery-url paste-id "/")
                             (concat "?" "api_key" "=" pastery-api-key)))
        (paste-buffer-name (concat "*pastery-" paste-id "*")))
    (pastery--request "GET" request-url nil
                      (cl-function
                       (lambda (&rest args &key error-thrown &allow-other-keys)
                         (print error-thrown)))
                      (cl-function
                       (lambda (&key data &allow-other-keys)
                         (pastery--place-buffer paste-buffer-name data))))
    (message (concat "Fetching paste " paste-id "..."))))

(provide 'pastery)
;;; pastery.el ends here
