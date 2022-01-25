;; All configuration related to setting up evil-mode and making sure
;; it plays nicely with other packages should go here

(use-package undo-tree)

;; Vim-like bindings
(use-package evil
  :after undo-tree
  :init
  (setq evil-want-keybinding nil)
  (setq evil-overriding-maps nil
        evil-intercept-maps nil)
  (setq evil-undo-system 'undo-tree)
  (setq evil-local-mode-hook 'turn-on-undo-tree-mode)
  :config (evil-mode t))
  ;;:general ('insert prog-mode-map "DEL" 'backward-delete-char-untabify))

;; Make vim-like bindings play nice everywhere
(use-package evil-collection
  :after evil
  :config (evil-collection-init))

(use-package evil-surround
  :after evil
  :defer
  :hook (prog-mode . evil-surround-mode)
  :hook (text-mode . evil-surround-mode))

(use-package god-mode
  :after evil
  :config
  (require 'evil-god-state)
  (general-def 'normal global-map "," 'evil-execute-in-god-state)
  (which-key-enable-god-mode-support))
