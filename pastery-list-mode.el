;;; pastery-list-mode.el --- paste snippets to pastery.net. -*- lexical-binding: t; -*-

;; Author: Bruno Dias <dias.h.bruno@gmail.com>
;; Version: 0.2.1
;; Package-Requires: ((emacs "24.4"))
;; Keywords: tools
;; Homepage: https://github.com/diasbruno/pastery.el

;; See license.md.

;; This file is NOT part of GNU Emacs.

;;; Code:

(require 'tabulated-list)

(defun pastery-list--refresh ()
  "Refresh the list of pastes."
  (interactive)
  (pastery-net--get-list-of-my-pastes
   (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                  (print error-thrown)))
   (cl-function (lambda (&key data &allow-other-keys)
                  (let* ((entries (cdar data)))
                    (progn
                      (when (eq (length entries) 0)
                        (message "No pastes."))
                      (pastery--make-tabular-list "*pastery-list*" entries)))))))

(defun pastery-list--delete-entry ()
  "Delete an entry from the table."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (when (not (null id))
      (pastery-net--delete-paste-by-id
       id
       (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                      (print error-thrown)))
       (cl-function (lambda (&key data &allow-other-keys)
                      (progn
                        (message "...done.")
                        (call-interactively #'pastery-list--refresh))))))))

(defun pastery-list--get-paste-of-entry ()
  "Get the paste on the current entry and open a buffer for it."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (when (not (null id))
      (pastery-net--get-paste-by-id
       id
       (cl-function
        (lambda (&rest args &key error-thrown &allow-other-keys)
          (print error-thrown)))
       (cl-function
        (lambda (&key data &allow-other-keys)
          (message "...done.")
          (pastery--open-paste-buffer id data)))))))

(defvar pastery-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "r" 'pastery-list--refresh)
    (define-key map "d" 'pastery-list--delete-entry)
    (define-key map "x" 'pastery-list--get-paste-of-entry)
    map)
  "Local keymap for `pastery-list-mode' buffers.")

(define-derived-mode
  pastery-list-mode tabulated-list-mode "pastery-list"
  "Major mode for browsing a list of pastes.
\\<pastery-list-mode-map>
\\{pastery-list-mode-map}"

  (setq tabulated-list-format
        `[("id" 18 nil)
          ("title" 13 nil)
          ("url" 10 nil)
          ("language" 20 nil)
          ("duration" 20 nil)
          ])
  (tabulated-list-init-header))

(provide 'pastery-list-mode)
;;; pastery-list-mode.el ends here
