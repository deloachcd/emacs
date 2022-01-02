;; All configuration related to setting up evil-mode and making sure
;; it plays nicely with other packages should go here

;; Vim-like bindings
(use-package evil
  :init
  (progn
    (setq evil-want-keybinding nil)
    (setq evil-overriding-maps nil
          evil-intercept-maps nil))
  :config (evil-mode t))

;; Make vim-like bindings play nice everywhere
(use-package evil-collection
  :config (evil-collection-init))

(use-package evil-surround
  :defer
  :hook (prog-mode . evil-surround-mode))

(use-package god-mode
  :after evil
  :config
  (require 'evil-god-state)
  (evil-define-key 'normal global-map "," 'evil-execute-in-god-state)
  (which-key-enable-god-mode-support))
