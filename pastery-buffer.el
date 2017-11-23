;;; pastery-buffer.el --- paste snippets to pastery.net. -*- lexical-binding: t; -*-

;; Author: Bruno Dias <dias.h.bruno@gmail.com>
;; Version: 0.2.1
;; Package-Requires: ((emacs "24.4"))
;; Keywords: tools
;; Homepage: https://github.com/diasbruno/pastery.el

;; Comments:

;; This file contains buffer related functions.

;; See license.md.

;; This file is NOT part of GNU Emacs.

;;; Code:

(defun pastery--paste-buffer-name (id)
  "Make the buffer's name for a paste using its ID."
  (concat "*pastery-" id "*"))

(defun pastery--region-substring (beg end)
  "Get the region if available BEG and END."
  (if (and beg end)
      (buffer-substring-no-properties beg end)
    ""))

(defun pastery--get-content ()
  "Get the content of the pastery from buffer or region."
  (if (region-active-p)
      (pastery--region-substring (point) (mark))
    (pastery--region-substring (point-min) (point-max))))

(defun pastery--make-paste-content (data)
  "Transform the DATA from a paste in to string."
  (let* ((paste-item (aref (cdar data) 0))
         (header (pastery--format-paste-header paste-item))
         (content (assoc 'body paste-item)))
    (concat header "\n\n"
            (pastery--check-content (cdr content))
            "\n\n")))

(defun pastery--open-paste-buffer (id data)
  "Open and write to a new buffer with name ID and DATA."
  (with-current-buffer (get-buffer-create (pastery--paste-buffer-name id))
    (erase-buffer)
    (insert (pastery--make-paste-content data))
    (view-buffer (current-buffer))
    (pop-to-buffer (current-buffer))))

(defun pastery--make-tabular-list (paste-buffer-name entries)
  "Open PASTE-BUFFER-NAME with a tabular list of ENTRIES."
  (with-current-buffer (get-buffer-create paste-buffer-name)
    (pastery-list-mode)
    (setf tabulated-list-entries (pastery--response-to-tabular-entries entries))
    (tabulated-list-print)
    (switch-to-buffer (current-buffer))))

(provide 'pastery-buffer)
;;; pastery-buffer.el ends here
