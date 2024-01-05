;; All configuration related to setting up evil-mode and making sure
;; it plays nicely with other packages should go here
(use-package undo-tree
  :config
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

;; Vim-like bindings
(use-package evil
  :after undo-tree
  :init
  (setq evil-want-keybinding nil)
  (setq evil-disable-insert-state-bindings t)
  (setq evil-overriding-maps nil
        evil-intercept-maps nil)
  (setq evil-undo-system 'undo-tree)
  (setq evil-local-mode-hook 'turn-on-undo-tree-mode)
  :config 
  (evil-mode t))

;; Make vim-like bindings play nice everywhere
(use-package evil-collection
  :after evil
  :config (evil-collection-init))

(use-package evil-surround
  :after evil
  :defer
  :hook (prog-mode . evil-surround-mode)
  :hook (text-mode . evil-surround-mode))
