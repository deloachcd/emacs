;; Configuration related to ivy/swiper/counsel, and which-key.
(provide 'minibuffer-completion-layer)

;; Ivy completion engine for minibuffer-driven user interaction,
;; counsel-mode pulls in ivy-specific replacements for emacs
;; system functions
(use-package counsel
  :config
  (ivy-mode 1))

;; Provides suggestions for available keybinds as you type
(use-package which-key
  :config (which-key-mode))
