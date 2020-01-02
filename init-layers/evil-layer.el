(provide 'evil-layer)

;; make sure nothing can override evil keybinds
(setq evil-overriding-maps nil
      evil-intercept-maps nil)

(require 'evil)
(require 'evil-leader)
(global-evil-leader-mode)
(evil-mode t)
