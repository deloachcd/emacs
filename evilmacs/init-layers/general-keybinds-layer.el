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
  "SPC" 'execute-extended-command)

;; TODO: SPC e as general eval layer, not just elisp
(general-create-definer elisp-bindings
  :prefix "SPC e"
  :states '(normal emacs visual)
  :keymaps 'override)

(elisp-bindings
  "" '(nil :which-key "eval")
  "b" 'eval-buffer
  "s" 'eval-last-sexp
  "r" 'eval-region
  "i" 'indent-pp-sexp)

(general-create-definer buffer-management-bindings
  :prefix "SPC b"
  :states '(normal emacs)
  :keymaps 'override)
(buffer-management-bindings
  "" '(nil :which-key "buffers")
  "b" 'consult-buffer
  "n" 'next-buffer
  "p" 'previous-buffer
  "k" 'kill-current-buffer
  "K" 'kill-buffer)

(defun sudo-find-file (file-name)
  "Like find file, but opens the file as root."
  (interactive "FFind file (sudo): ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (find-file tramp-file-name)))

(general-create-definer file-bindings
  :prefix "SPC f"
  :states '(normal emacs)
  :keymaps 'override)
(file-bindings
  "" '(nil :which-key "files")
  "f" 'find-file
  "s" 'sudo-find-file)

(general-create-definer help-bindings
  :prefix "SPC h"
  :states '(normal emacs)
  :keymaps 'override)

;; NOTE: some of these get overriden if 'helpful' layer gets loaded after
(help-bindings
  "" '(nil :which-key "help")
  "c" 'describe-char
  "k" 'describe-key
  "f" 'describe-function
  "v" 'describe-variable
  "a" 'apropos)

;; NOTE: indent-whole-buffer defined in editing layer
(general-create-definer indentation-bindings
  :prefix "SPC i"
  :states '(normal emacs visual)
  :keymaps 'override)
(indentation-bindings
  "" '(nil :which-key "indent")
  "r" 'indent-region
  "b" 'indent-whole-buffer)

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

(general-create-definer tab-management-bindings
  :prefix "SPC t"
  :states '(normal emacs)
  :keymaps 'override)
(tab-management-bindings
  "" '(nil :which-key "tabs")
  "n" 'tab-next
  "p" 'tab-previous
  "N" 'tab-new
  "k" 'smart-kill-current-tab
  )

;; this definer gets used later, to create keybinds within
;; language specific configuration layers
(general-create-definer language-bindings
  :prefix "SPC l"
  :states '(normal emacs)
  :keymaps 'override)

(language-bindings
  "" '(nil :which-key "lang"))
