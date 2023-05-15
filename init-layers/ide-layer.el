;; This layer is for two things:
;; 1. Configuration related to lsp-mode
;; 2. Configuration related to specific languages & their modes
;;
;; Anything configuration that depends on lsp-mode needs to
;; `require' this one.
(provide 'ide-layer)

;; Primary autocompletion engine
(use-package lsp-mode
  :defer
  :commands lsp)
(use-package lsp-ui
  :defer
  :hook (lsp-ui-mode . (lambda () (progn
                               (setq lsp-ui-doc-enable nil)
                               (setq lsp-ui-doc-position 'bottom)
                               (setq lsp-ui-doc-alignment 'window))))
  :commands lsp-ui-mode)

(defun lsp-init ()
  (interactive)
  (lsp)
  (lsp-ui-mode))

;; C/C++ specific configuration
(use-package ccls
  :defer)
(setq ccls-executable "/usr/bin/ccls")
(setq c-basic-offset 4
      c-default-style "stroustrup")
(require 'cmake-mode)
(require 'glsl-mode)

(defun c-mode-hook-additions ()
  (electric-pair-local-mode))

(add-hook 'c-mode-hook 'c-mode-hook-additions)
(add-hook 'c++-mode-hook 'c-mode-hook-additions)

;; Python specific configuration
(defun python-mode-hook-additions ()
  (electric-pair-local-mode))

(add-hook 'python-mode-hook 'python-mode-hook-additions)
(setq org-babel-python-command "/usr/bin/env python3")
