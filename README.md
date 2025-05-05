# tab-bar-extensions

Enhance your Emacs experience with smarter tab switching, tab grouping, and interactive navigation — all built on top of Emacs’ built-in `tab-bar` feature.

## ✨ Features

- 🚀 Switch to the next/previous tab **within the current group**
- 🎯 Select tabs interactively via `completing-read`
- 🗂️ Group tabs logically (ungrouped tabs automatically go to a `"Default"` group)
- 🎛️ Customizable settings for messages, group names, and keybindings

## 📦 Installation

### Using `use-package` (recommended)

```elisp
(use-package tab-bar-extensions
  :load-path "~/path/to/TGE.el"  ;; Adjust to where the file is
  :custom
  (tab-bar-extensions-default-group-name "My Default Group") ;; optional
  (tab-bar-extensions-next-tab-message "Now at tab: %s")     ;; optional
  :config
  (tab-bar-extensions-mode 1))
```

### Manual Setup

```elisp
(add-to-list 'load-path "~/path/to/TGE.el")
(require 'tab-bar-extensions)

;; Optional customization
(setq tab-bar-extensions-default-group-name "My Default Group")
(setq tab-bar-extensions-next-tab-message "Switched to %s")

(tab-bar-extensions-mode 1)
```

## 🎛️ Customization

| Variable                                 | Description |
|------------------------------------------|-------------|
| `tab-bar-extensions-default-group-name`  | Name for tabs without an explicit group (default: `"Default"`) |
| `tab-bar-extensions-next-tab-message`    | Message shown when switching tabs (can include `%s` for tab name) |


## 🎹 Keybindings

> 🔐 **No keys are bound by default.** You define your own.

```elisp
(defvar tab-bar-extensions-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'tab-bar-extensions-switch-to-next-tab)
    (define-key map (kbd "p") #'tab-bar-extensions-switch-to-previous-tab)
    (define-key map (kbd "s") #'tab-bar-extensions-select-tab)
    (define-key map (kbd "g") #'tab-bar-extensions-select-group-and-tab)
    (define-key map (kbd "d") #'tab-bar-extensions-delete-group-tabs)
    map)
  "Keymap for tab-bar-extensions commands.")
  
;; Bind the keymap to a prefix key
(global-set-key (kbd "C-c W") tab-bar-extensions-keymap)
```

This example binds all commands to the `C-c W` prefix key.


## 🚀 Interactive Commands

| Command                                     | Description |
|--------------------------------------------|-------------|
| `tab-bar-extensions-switch-to-next-tab`     | Switch to next tab in the current group |
| `tab-bar-extensions-switch-to-previous-tab` | Switch to previous tab in the current group |
| `tab-bar-extensions-select-tab`             | Interactively select a tab from the current group |
| `tab-bar-extensions-select-group-and-tab`   | Select a group first, then a tab |
| `tab-bar-extensions-delete-group-tabs`      | Delete a tab group and all tabs within |
| `tab-bar-extensions-delete-some-tab-groups` | Delete some tab groups |
| `tab-bar-extensions-delete-other-tab-groups`| Delete all tab groups except the current one |
