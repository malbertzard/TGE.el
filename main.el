;;; -*- lexical-binding: t -*-
;;; tab-bar-extensions.el --- Extensions for working with the tab bar in Emacs

;; Author: Mathis Albertzard <malbertzard@gmail.com>
;; URL: https://github.com/malbertzard/TGE
;; Package-Version: 1.0.0
;; Keywords: emacs, tabs, tab-bar
;; License: GPL-3.0-or-later

;;; Commentary:
;; This package provides extensions for working with Emacs' tab-bar feature.
;; It includes functions for switching tabs, selecting tabs interactively, 
;; and managing tab groups.

;;; Code:

(require 'tab-bar)
(require 'cl-lib)
(require 'seq)

(defgroup tab-bar-extensions nil
  "Extensions for working with the tab bar in Emacs."
  :group 'tools)

(defcustom tab-bar-extensions-next-tab-message "Switched to tab: %s"
  "Message displayed when switching to a tab."
  :type 'string
  :group 'tab-bar-extensions)

(defcustom tab-bar-extensions-prefix-key (kbd "C-c t")
  "Prefix key for `tab-bar-extensions-mode` keybindings."
  :type 'key-sequence
  :group 'tab-bar-extensions)

(defun tab-bar-extensions--get-current-tab (&optional frame)
  "Retrieve the current tab for FRAME, defaulting to `selected-frame`."
  (let* ((tabs (tab-bar-tabs frame))
         (current-tab (tab-bar--current-tab-find tabs)))
    (or current-tab
        (progn
          (warn "No current tab found in the specified frame.")
          nil))))

(defun tab-bar-extensions--get-current-group (current-tab)
  "Retrieve the group associated with CURRENT-TAB."
  (cdr (assq 'group current-tab)))

(defun tab-bar-extensions--get-group-tabs (group tabs)
  "Return tabs from TABS that belong to GROUP."
  (seq-filter
   (lambda (tab) (equal (cdr (assq 'group tab)) group))
   tabs))

(defun tab-bar-extensions--get-tab-names (group-tabs)
  "Return a list of tab names from GROUP-TABS."
  (mapcar (lambda (tab) (cdr (assq 'name tab))) group-tabs))

(defun tab-bar-extensions--find-tab-index (tab-names current-name)
  "Return index of CURRENT-NAME in TAB-NAMES."
  (seq-position tab-names current-name #'equal))

(defun tab-bar-extensions--switch-to-tab (name)
  "Switch to tab NAME and show a confirmation message."
  (tab-bar-switch-to-tab name)
  (message tab-bar-extensions-next-tab-message (propertize name 'face 'bold)))

(defun tab-bar-extensions--switch-to-next-tab (&optional frame)
  "Switch to the next tab in FRAME."
  (let* ((tabs (tab-bar-tabs frame))
         (current-tab (tab-bar-extensions--get-current-tab frame))
         (group (tab-bar-extensions--get-current-group current-tab))
         (group-tabs (tab-bar-extensions--get-group-tabs group tabs))
         (tab-names (tab-bar-extensions--get-tab-names group-tabs))
         (current-name (cdr (assq 'name current-tab)))
         (current-index (tab-bar-extensions--find-tab-index tab-names current-name))
         (next-index (mod (+ current-index 1) (length tab-names))))
    (when (and tab-names current-index)
      (tab-bar-extensions--switch-to-tab (nth next-index tab-names)))))

(defun tab-bar-extensions--switch-to-previous-tab (&optional frame)
  "Switch to the previous tab in FRAME."
  (let* ((tabs (tab-bar-tabs frame))
         (current-tab (tab-bar-extensions--get-current-tab frame))
         (group (tab-bar-extensions--get-current-group current-tab))
         (group-tabs (tab-bar-extensions--get-group-tabs group tabs))
         (tab-names (tab-bar-extensions--get-tab-names group-tabs))
         (current-name (cdr (assq 'name current-tab)))
         (current-index (tab-bar-extensions--find-tab-index tab-names current-name))
         (prev-index (mod (- current-index 1) (length tab-names))))
    (when (and tab-names current-index)
      (tab-bar-extensions--switch-to-tab (nth prev-index tab-names)))))

(defun tab-bar-extensions-get-current-group-name ()
  "Return name of the current tab group."
  (let* ((current-tab (tab-bar-extensions--get-current-tab))
         (group (tab-bar-extensions--get-current-group current-tab)))
    (if group
        (cdr (assq 'name group))
      "No group")))

(defun tab-bar-extensions--list-all-tab-groups ()
  "Return a list of all tab groups in the current frame."
  (let* ((tabs (tab-bar-tabs)))
    (seq-uniq (mapcar (lambda (tab) (cdr (assq 'group tab))) tabs))))

;;; Interactive Commands

;;;###autoload
(defun tab-bar-extensions-select-tab ()
  "Select a tab from the current group."
  (interactive)
  (let* ((tabs (tab-bar-tabs))
         (current-tab (tab-bar-extensions--get-current-tab))
         (group (tab-bar-extensions--get-current-group current-tab))
         (group-tabs (tab-bar-extensions--get-group-tabs group tabs))
         (tab-names (tab-bar-extensions--get-tab-names group-tabs)))
    (if tab-names
        (let ((selected-tab (completing-read "Select tab: " tab-names nil t)))
          (tab-bar-extensions--switch-to-tab selected-tab))
      (message "No tabs available in this group."))))

;;;###autoload
(defun tab-bar-extensions-select-group-and-tab ()
  "Select a group, then a tab from that group."
  (interactive)
  (let* ((tabs (tab-bar-tabs))
         (groups (seq-uniq (mapcar (lambda (tab) (cdr (assq 'group tab))) tabs)))
         (selected-group (completing-read "Select a tab group: " groups nil t))
         (group-tabs (tab-bar-extensions--get-group-tabs selected-group tabs))
         (tab-names (tab-bar-extensions--get-tab-names group-tabs)))
    (if tab-names
        (let ((selected-tab (completing-read "Select tab: " tab-names nil t)))
          (tab-bar-extensions--switch-to-tab selected-tab))
      (message "No tabs available in the selected group."))))

;;;###autoload
(defun tab-bar-extensions-switch-to-next-tab ()
  "Interactive command to switch to the next tab."
  (interactive)
  (tab-bar-extensions--switch-to-next-tab))

;;;###autoload
(defun tab-bar-extensions-switch-to-previous-tab ()
  "Interactive command to switch to the previous tab."
  (interactive)
  (tab-bar-extensions--switch-to-previous-tab))

;;;###autoload
(defvar tab-bar-extensions-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'tab-bar-extensions-switch-to-next-tab)
    (define-key map (kbd "p") #'tab-bar-extensions-switch-to-previous-tab)
    (define-key map (kbd "s") #'tab-bar-extensions-select-tab)
    (define-key map (kbd "g") #'tab-bar-extensions-select-group-and-tab)
    map)
  "Keymap for `tab-bar-extensions-mode`.")

;;;###autoload
(define-minor-mode tab-bar-extensions-mode
  "Global minor mode for tab-bar-extensions keybindings."
  :global t
  :lighter " TBE"
  :keymap (let ((prefix-map (make-sparse-keymap)))
            (define-key prefix-map tab-bar-extensions-prefix-key tab-bar-extensions-mode-map)
            prefix-map))

(provide 'tab-bar-extensions)

;;; tab-bar-extensions.el ends here
