;;; tab-bar-extensions.el --- Extensions for working with the tab bar in Emacs

;; Author: Your Name <your.email@example.com>
;; URL: https://your-package-repository-url
;; Package-Version: 1.0.0
;; Keywords: emacs, tabs, tab-bar
;; License: GPL-3.0-or-later

;;; Commentary:
;; This package provides extensions for working with Emacs' tab-bar feature.
;; It includes functions for switching tabs, selecting tabs interactively, 
;; and managing tab groups.

;;; Code:

(defgroup tab-bar-extensions nil
  "Extensions for working with the tab bar in Emacs."
  :group 'tools)

(defcustom tab-bar-extensions-next-tab-message "Switching to the next tab."
  "Message displayed when switching to the next tab."
  :type 'string
  :group 'tab-bar-extensions)

(defun tab-bar-extensions--get-current-tab (frame)
  "Retrieve the current tab for the given FRAME.
If no current tab is found, a warning will be issued and nil will be returned."
  (let* ((tabs (tab-bar-tabs))
         (current-tab (tab-bar--current-tab-find tabs)))
    (if current-tab
        current-tab
      (progn
        (warn "No current tab found in the specified frame.")
        nil))))

(defun tab-bar-extensions--get-current-group (current-tab)
  "Retrieve the group associated with the CURRENT-TAB."
  (cdr (assq 'group current-tab)))

(defun tab-bar-extensions--get-group-tabs (group frame)
  "Retrieve all tabs in the specified GROUP for the given FRAME.
Returns a list of tabs belonging to the GROUP."
  (let ((tabs (tab-bar-tabs)))
    (cl-remove-if-not
     (lambda (tab) (equal (cdr (assq 'group tab)) group))
     tabs)))

(defun tab-bar-extensions--get-tab-names (group-tabs)
  "Return a list of tab names from GROUP-TABS."
  (mapcar (lambda (tab) (cdr (assq 'name tab))) group-tabs))

(defun tab-bar-extensions--find-tab-index (tab-names current-name)
  "Find the index of CURRENT-NAME in the TAB-NAMES list.
Returns the index if found, otherwise nil."
  (cl-position current-name tab-names :test 'equal))

(defun tab-bar-extensions--switch-to-next-tab (frame)
  "Switch to the next tab in the given FRAME and display a message."
  (let* ((current-tab (tab-bar-extensions--get-current-tab frame))
         (group (tab-bar-extensions--get-current-group current-tab))
         (group-tabs (tab-bar-extensions--get-group-tabs group frame))
         (tab-names (tab-bar-extensions--get-tab-names group-tabs))
         (current-name (cdr (assq 'name current-tab)))
         (current-index (tab-bar-extensions--find-tab-index tab-names current-name))
         (next-index (if current-index
                         (mod (+ current-index 1) (length tab-names))
                       0)))
    (when (and current-index (< next-index (length tab-names)))
      (let ((next-tab-name (nth next-index tab-names)))
        (tab-bar-switch-to-tab next-tab-name)
        (message tab-bar-extensions-next-tab-message)))))

(defun tab-bar-extensions--switch-to-previous-tab (frame)
  "Switch to the previous tab in the given FRAME and display a message."
  (let* ((current-tab (tab-bar-extensions--get-current-tab frame))
         (group (tab-bar-extensions--get-current-group current-tab))
         (group-tabs (tab-bar-extensions--get-group-tabs group frame))
         (tab-names (tab-bar-extensions--get-tab-names group-tabs))
         (current-name (cdr (assq 'name current-tab)))
         (current-index (tab-bar-extensions--find-tab-index tab-names current-name))
         (prev-index (if current-index
                         (mod (- current-index 1) (length tab-names))
                       0)))
    (when (and current-index (>= prev-index 0))
      (let ((prev-tab-name (nth prev-index tab-names)))
        (tab-bar-switch-to-tab prev-tab-name)
        (message tab-bar-extensions-next-tab-message)))))

(defun tab-bar-extensions-get-current-group-name ()
  "Get the current tab group's name."
  (let* ((current-tab (tab-bar-extensions--get-current-tab (selected-frame)))
         (group (tab-bar-extensions--get-current-group current-tab)))
    (if group
        (cdr (assq 'name group))
      "No group")))

(defun tab-bar-extensions--list-all-tab-groups ()
  "Return a list of all tab groups in the current frame."
  (let* ((tabs (tab-bar-tabs))  ;; Get all tabs in the current frame
         (groups (seq-uniq (mapcar (lambda (tab) (cdr (assq 'group tab))) tabs))))  ;; Extract unique groups
    groups))

;;; Interactive Commands

;;;###autoload
(defun tab-bar-extensions-select-tab ()
  "Interactive command to select a tab inside the current group."
  (interactive)
  (let* ((current-tab (tab-bar-extensions--get-current-tab (selected-frame)))
         (group (tab-bar-extensions--get-current-group current-tab))
         (group-tabs (tab-bar-extensions--get-group-tabs group (selected-frame)))
         (tab-names (tab-bar-extensions--get-tab-names group-tabs)))
    (if tab-names
        (let* ((selected-tab (completing-read "Select tab: " tab-names nil t))
               (tab-to-switch (cl-find selected-tab tab-names :test 'equal)))
          (when tab-to-switch
            (tab-bar-switch-to-tab selected-tab)
            (message "Switched to tab: %s" selected-tab)))
      (message "No tabs available in this group."))))

;;;###autoload
(defun tab-bar-extensions-select-group-and-tab ()
  "Interactive command to select a tab group, then a tab inside that group."
  (interactive)
  (let* ((groups (tab-bar-extensions--list-all-tab-groups))
         (selected-group (completing-read "Select a tab group: " groups nil t))
         (group-tabs (tab-bar-extensions--get-group-tabs selected-group (selected-frame)))
         (tab-names (tab-bar-extensions--get-tab-names group-tabs)))
    (if (and selected-group tab-names)
        (let* ((selected-tab (completing-read "Select a tab: " tab-names nil t))
               (tab-to-switch (cl-find selected-tab tab-names :test 'equal)))
          (when tab-to-switch
            (tab-bar-switch-to-tab selected-tab)
            (message "Switched to tab: %s" selected-tab)))
      (message "No tabs available in the selected group."))))
;;;###autoload

(defun tab-bar-extensions-switch-to-next-tab ()
  "Interactive command to switch to the next tab in the current frame."
  (interactive)
  (tab-bar-extensions--switch-to-next-tab (selected-frame)))

;;;###autoload
(defun tab-bar-extensions-switch-to-previous-tab ()
  "Interactive command to switch to the previous tab in the current frame."
  (interactive)
  (tab-bar-extensions--switch-to-previous-tab (selected-frame)))


(provide 'tab-bar-extensions)
;;; tab-bar-extensions.el ends here
