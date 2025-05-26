;;; -*- lexical-binding: t -*-
;;; tab-group-extensions.el --- Extensions for working with the tab bar in Emacs

;; Author: Mathis Albertzard <malbertzard@gmail.com>
;; URL: https://github.com/malbertzard/TGE
;; Package-Version: 1.0.0
;; Keywords: emacs, tabs, tab-bar
;; License: GPL-3.0-or-later

;;; Commentary:
;; This package provides extensions for working with Emacs' tab-group feature.
;; It includes functions for switching tabs, selecting tabs interactively, 
;; and managing tab groups.

;;; Code:

(require 'tab-bar)
(require 'cl-lib)
(require 'seq)

(defgroup tab-group-extensions nil
  "Extensions for working with the tab bar in Emacs."
  :group 'tools)

(defcustom tab-group-extensions-next-tab-message "Switched to tab: %s"
  "Message displayed when switching to a tab."
  :type 'string
  :group 'tab-bar-extensions)

(defcustom tab-group-extensions-default-group-name "Default"
  "Name used for tabs that don't belong to any group."
  :type 'string
  :group 'tab-bar-extensions)

(defun tab-group-extensions--get-current-tab (&optional frame)
  "Retrieve the current tab for FRAME, defaulting to `selected-frame`."
  (pcase (tab-bar--current-tab-find (tab-group-tabs frame))
    (`nil
     (warn "No current tab found in the specified frame.")
     nil)
    (tab tab)))

(defun tab-group-extensions--get-current-group (current-tab)
  "Retrieve the group name associated with CURRENT-TAB."
  (or (cdr (assq 'group current-tab))
      tab-group-extensions-default-group-name))

(defun tab-group-extensions--get-group-tabs (group tabs)
  "Return tabs from TABS that belong to GROUP.
Tabs without a group are included under `tab-group-extensions-default-group-name`."
  (seq-filter
   (lambda (tab)
     (let ((tab-group (or (cdr (assq 'group tab)) tab-group-extensions-default-group-name)))
       (equal tab-group group)))
   tabs))

(defun tab-group-extensions--delete-group-tabs (group)
  "Delete all tabs in the tab group GROUP."
  (let* ((tabs (tab-group-tabs))
         (group-tabs (tab-group-extensions--get-group-tabs group tabs)))
    (dolist (tab group-tabs)
      (let ((name (cdr (assq 'name tab))))
        (tab-group-close-tab-by-name name)))))

(defun tab-group-extensions--get-tab-names (group-tabs)
  "Return a list of tab names from GROUP-TABS."
  (mapcar (lambda (tab) (cdr (assq 'name tab))) group-tabs))

(defun tab-group-extensions--find-tab-index (tab-names current-name)
  "Return index of CURRENT-NAME in TAB-NAMES."
  (seq-position tab-names current-name #'equal))

(defun tab-group-extensions--switch-to-tab (name)
  "Switch to tab NAME and show a confirmation message."
  (tab-group-switch-to-tab name)
  (message tab-group-extensions-next-tab-message (propertize name 'face 'bold)))

(defun tab-group-extensions--switch-to-next-tab (&optional frame)
  "Switch to the next tab in FRAME."
  (let* ((tabs (tab-group-tabs frame))
         (current-tab (tab-group-extensions--get-current-tab frame))
         (group (tab-group-extensions--get-current-group current-tab))
         (group-tabs (tab-group-extensions--get-group-tabs group tabs))
         (tab-names (tab-group-extensions--get-tab-names group-tabs))
         (current-name (cdr (assq 'name current-tab)))
         (current-index (tab-group-extensions--find-tab-index tab-names current-name)))
    (when (and tab-names (not (null tab-names)) current-index)
      (let ((next-index (mod (+ current-index 1) (length tab-names))))
        (tab-group-extensions--switch-to-tab (nth next-index tab-names))))))

(defun tab-group-extensions--switch-to-previous-tab (&optional frame)
  "Switch to the previous tab in FRAME."
  (let* ((tabs (tab-group-tabs frame))
         (current-tab (tab-group-extensions--get-current-tab frame))
         (group (tab-group-extensions--get-current-group current-tab))
         (group-tabs (tab-group-extensions--get-group-tabs group tabs))
         (tab-names (tab-group-extensions--get-tab-names group-tabs))
         (current-name (cdr (assq 'name current-tab)))
         (current-index (tab-group-extensions--find-tab-index tab-names current-name)))
    (when (and tab-names (not (null tab-names)) current-index)
      (let ((prev-index (mod (- current-index 1) (length tab-names))))
        (tab-group-extensions--switch-to-tab (nth prev-index tab-names))))))

(defun tab-group-extensions-get-current-group-name ()
  "Return name of the current tab group."
  (let* ((current-tab (tab-group-extensions--get-current-tab))
         (group (tab-group-extensions--get-current-group current-tab)))
    group))

(defun tab-group-extensions--list-all-tab-groups ()
  "Return a list of all tab groups in the current frame.
Tabs without a group are grouped under `tab-group-extensions-default-group-name`."
  (let* ((tabs (tab-group-tabs))
         (groups (mapcar (lambda (tab)
                           (or (cdr (assq 'group tab)) tab-group-extensions-default-group-name))
                         tabs)))
    (seq-uniq groups)))

;;; Interactive Commands

;;;###autoload
(defun tab-group-extensions-select-tab ()
  "Select a tab from the current group."
  (interactive)
  (let* ((tabs (tab-group-tabs))
         (current-tab (tab-group-extensions--get-current-tab))
         (group (tab-group-extensions--get-current-group current-tab))
         (group-tabs (tab-group-extensions--get-group-tabs group tabs))
         (tab-names (tab-group-extensions--get-tab-names group-tabs)))
    (if tab-names
        (let ((selected-tab (completing-read "Select tab: " tab-names nil t)))
          (tab-group-extensions--switch-to-tab selected-tab))
      (message "No tabs available in this group."))))

;;;###autoload
(defun tab-group-extensions-select-group-and-tab ()
  "Select a group, then a tab from that group."
  (interactive)
  (let* ((tabs (tab-group-tabs))
         (groups (tab-group-extensions--list-all-tab-groups))
         (selected-group (completing-read "Select a tab group: " groups nil t))
         (group-tabs (tab-group-extensions--get-group-tabs selected-group tabs))
         (tab-names (tab-group-extensions--get-tab-names group-tabs)))
    (if tab-names
        (let ((selected-tab (completing-read "Select tab: " tab-names nil t)))
          (tab-group-extensions--switch-to-tab selected-tab))
      (message "No tabs available in the selected group."))))

;;;###autoload
(defun tab-group-extensions-switch-to-next-tab ()
  "Interactive command to switch to the next tab."
  (interactive)
  (tab-group-extensions--switch-to-next-tab))

;;;###autoload
(defun tab-group-extensions-delete-group-tabs ()
  "Interactively select a group and delete all tabs in that group."
  (interactive)
  (let* ((groups (tab-group-extensions--list-all-tab-groups))
         (selected-group (completing-read "Select tab group to delete: " groups nil t)))
    (when (yes-or-no-p (format "Really delete all tabs in group '%s'?" selected-group))
      (tab-group-extensions--delete-group-tabs selected-group)
      (message "Deleted all tabs in group '%s'" selected-group))))


;;;###autoload
(defun tab-group-extensions-switch-to-previous-tab ()
  "Interactive command to switch to the previous tab."
  (interactive)
  (tab-group-extensions--switch-to-previous-tab))

;;;###autoload
(defun tab-group-extensions-delete-some-tab-groups ()
  "Interactively go through all tab groups and ask whether to delete them."
  (interactive)
  (let ((groups (tab-group-extensions--list-all-tab-groups)))
    (dolist (group groups)
      (when (yes-or-no-p (format "Delete all tabs in group '%s'? " group))
        (tab-group-extensions--delete-group-tabs group)
        (message "Deleted all tabs in group '%s'" group)))))

;;;###autoload
(defun tab-group-extensions-delete-other-tab-groups ()
  "Delete all tab groups except the current one."
  (interactive)
  (let* ((current-tab (tab-group-extensions--get-current-tab))
         (current-group (tab-group-extensions--get-current-group current-tab))
         (all-groups (tab-group-extensions--list-all-tab-groups))
         (other-groups (seq-remove (lambda (group)
                                     (equal group current-group))
                                   all-groups)))
    (when (yes-or-no-p
           (format "Really delete all tabs except those in group '%s'? " current-group))
      (dolist (group other-groups)
        (tab-group-extensions--delete-group-tabs group))
      (message "Deleted all tabs in groups other than '%s'" current-group))))

;;;###autoload
(define-minor-mode tab-group-extensions-mode
  "Global minor mode for tab-group-extensions keybindings."
  :global t
  :lighter "TBE")

(provide 'tab-group-extensions)

;;; tab-group-extensions.el ends here
