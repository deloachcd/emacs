(use-package vterm)

(require 'general-keybinds-layer)
(general-create-definer shell-bindings
  :prefix "SPC s"
  :states '(normal emacs visual)
  :keymaps 'override)
(shell-bindings
  "" '(nil :which-key "shell")
  "m" 'sh-mode
  "e" 'eshell
  "s" 'vterm)

;; useful for bailing out of nano, when git merge pulls it up
(general-def 'normal vterm-mode-map "X" 'vterm-send-C-x)

;; NOTE
;; this works, but I don't think flycheck is really worth it for
;; just checking simple bash syntax errors at this point. maybe
;; shellcheck will make it worth it if I ever want to clean up
;; my scripts
;;
;;(require 'flycheck-layer)
;;(setq flycheck-sh-bash-executable "/usr/bin/bash")
;;(setq flycheck-sh-zsh-executable "/usr/bin/zsh")
;;(defun sh-mode-enable-flycheck-h ()
;;  "Enable flycheck for sh buffers"
;;  (flycheck-mode)
;;  (if (string-match-p (regexp-quote "\.zsh") ".zshrc")
;;      (flycheck-select-checker 'sh-zsh)
;;    (flycheck-select-checker 'sh-bash)))
;; TODO: set up shellcheck and see why this isn't working
;;(add-hook 'sh-mode-hook 'sh-mode-enable-flycheck-h)
