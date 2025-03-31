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
  (let* ((tabs (tab-bar-tabs-in-current-group frame))
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
  (tab-bar-tabs-in-group group frame))

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

;;; Interactive Commands

(defun tab-bar-extensions-switch-to-next-tab ()
  "Interactive command to switch to the next tab in the current frame."
  (interactive)
  (tab-bar-extensions--switch-to-next-tab (selected-frame)))

(defun tab-bar-extensions-switch-to-previous-tab ()
  "Interactive command to switch to the previous tab in the current frame."
  (interactive)
  (tab-bar-extensions--switch-to-previous-tab (selected-frame)))

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


(provide 'tab-bar-extensions)


