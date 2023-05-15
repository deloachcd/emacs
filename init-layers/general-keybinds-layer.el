;; This layer provides SPC bindings for many common "emacs system"
;; bindings prefixed by C-x, along with general.el for defining
;; keybindings related to a specific configuration layer's
;; provided functionality.
;;
;; Every configuration layer that defines its own bindings with
;; general.el should `require' this layer
(provide 'general-keybinds-layer)

;; Keybindings not in this file:
;; org-mode and org-roam bindings are defined in org-layer.el
;; magit bindings are defined in magit-layer.el

;; Bail out of prompts with ESC
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; General provides a unified interface for key definitions,
;; perfect for spacemacs-style bindings with evil mode
(use-package general)

;; Provides suggestions for available keybinds as you type,
;; integrates with general.el
(use-package which-key
  :config (which-key-mode))

(general-create-definer root-bindings
  :prefix "SPC"
  :states '(normal emacs visual)
  :keymaps 'override)

(root-bindings
  ;;"SPC" 'execute-extended-command
  "m" '(nil :which-key "method"))

;; god-mode acts weird with C-x C-e for some reason, so I bind this
(general-define-key :states 'normal :keymaps 'emacs-lisp-mode-map
                    "SPC m e" 'eval-last-sexp)

(defun sudo-find-file (file-name)
  "Like find file, but opens the file as root."
  (interactive "FFind file (sudo): ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (find-file tramp-file-name)))

(general-create-definer window-management-bindings
  :prefix "SPC w"
  :states '(normal emacs)
  :keymaps 'override)
(window-management-bindings
  "" '(nil :which-key "window")
  "v" 'split-window-right
  "s" 'split-window-below
  "h" 'windmove-left
  "j" 'windmove-down
  "k" 'windmove-up
  "l" 'windmove-right)

;; Uses tab bar system built-in to emacs 27
(defun smart-kill-current-tab ()
  "Close current tab, and hide the tab bar if we only have one left."
  (interactive)
  (tab-close)
  (if (< (length (tab-bar-tabs)) 2)
      (tab-bar-mode -1)))
