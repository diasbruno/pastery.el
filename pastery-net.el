;;; pastery-net.el --- paste snippets to pastery.net. -*- lexical-binding: t; -*-

;; Author: Bruno Dias <dias.h.bruno@gmail.com>
;; Version: 0.2.1
;; Package-Requires: ((emacs "24.4"))
;; Keywords: tools
;; Homepage: https://github.com/diasbruno/pastery.el

;; Comments:

;; This file contains the net part of the package.

;; See license.md.

;; This file is NOT part of GNU Emacs.

;;; Code:

(require 'url-util)

(defun pastery-net--query-item (key value)
  "Create a query item for KEY and VALUE only if VALUE is not null."
  (if value
      (concat (symbol-name key) "=" (url-encode-url value))
    ""))

(defun pastery-net--build-query (items)
  "Build the query string for the api request using ITEMS."
  (mapconcat (lambda (x) (pastery-net--query-item (car x) (cdr x)))
             items
             "&"))

(defun pastery-net--url-for-paste (id)
  "Make the url for paste with ID."
  (concat pastery-url id "/" "?" "api_key" "=" pastery-api-key))

(defun pastery-net--request (method url data err succ)
  "Make the request to the api."
  (setf url-request-data data)
  (setf url-request-method method)
  (url-retrieve url
                (cl-function (lambda (response data)
                               (let ((status (car response)))
                                 (progn
                                   (print "DATA:")
                                   (print data)
                                   (print "RESPONSE")
                                   (print response)
                                   ))))
                nil
                t))

(defun pastery-net--get-list-of-my-pastes (err-cb succ-cb)
  "Fetch all available pastes.  Use callbacks ERR-CB and SUCC-CB."
  (let ((request-url (concat pastery-url "?api_key=" pastery-api-key)))
    (progn
      (message "Fetching list of pastes...")
      (pastery-net--request "GET" request-url nil err-cb succ-cb))))

(defun pastery-net--submit-paste (url data err-cb succ-cb)
  "Submit a new paste with URL and DATA.  Use callbacks ERR-CB and SUCC-CB."
  (progn
    (message (concat "Sending new paste..."))
    (pastery-net--request "POST" url data err-cb succ-cb)))

(defun pastery-net--get-paste-by-id (id err-cb succ-cb)
  "Get a paste by ID.  Use callbacks ERR-CB and SUCC-CB."
  (let ((request-url (pastery-net--url-for-paste id)))
    (progn
      (message (concat "Fetching paste " id "..."))
      (pastery-net--request "GET" request-url nil err-cb succ-cb))))

(defun pastery-net--delete-paste-by-id (id err-cb succ-cb)
  "Delete a paste with ID.  Use callbacks ERR-CB and SUCC-CB."
  (let ((request-url (pastery-net--url-for-paste id)))
    (progn
      (message (concat "Deleting paste " id "..."))
      (pastery-net--request "DELETE" request-url nil err-cb succ-cb))))


(provide 'pastery-net)
;;; pastery-net.el ends here
