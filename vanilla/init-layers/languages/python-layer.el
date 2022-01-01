;; configuration for an obscure little programming language called 'python'
(setq python-indent-offset 4)
(setq python-indent-guess-indent-offset-verbose nil)
(setq org-babel-python-command "/usr/bin/env python3")

(require 'general-keybinds-layer)
(language-bindings
  ;; Python bindings
  "p" '(nil :which-key "python")
  "p r" 'run-python
  "p m" 'python-mode
  "p s" '(nil :which-key "shell")
  "p s b" 'python-shell-send-buffer
  "p s r" 'python-shell-send-region)

(require 'flycheck-layer)
(defun python-mode-enable-flycheck-h ()
  "Enable flycheck for python buffers"
  (flycheck-mode)
  (flycheck-select-checker 'python-pycompile))
(add-hook 'python-mode-hook 'python-mode-enable-flycheck-h)
