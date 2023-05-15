;; configuration for an obscure little programming language called 'python'
(setq python-indent-offset 4)
(setq python-indent-guess-indent-offset-verbose nil)
(setq org-babel-python-command "/usr/bin/env python3")

;; automatically format code with black on save
(use-package blacken
  :hook (python-mode . blacken-mode))

(require 'flycheck-layer)
(defun python-mode-enable-flycheck-h ()
  "Enable flycheck for python buffers"
  (flycheck-mode)
  (flycheck-select-checker 'python-pycompile))
(add-hook 'python-mode-hook 'python-mode-enable-flycheck-h)
