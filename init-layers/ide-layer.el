;; This layer is for two things:
;; 1. Configuration related to lsp-mode
;; 2. Configuration related to specific languages & their modes
;;
;; Anything configuration that depends on lsp-mode needs to
;; `require' this one.
(provide 'ide-layer)

;; Primary autocompletion engine
(use-package company)
(use-package lsp-mode :commands lsp)
(use-package lsp-ui
  :hook (lsp-ui-mode . (lambda () (progn
                               (setq lsp-ui-doc-enable nil)
                               (setq lsp-ui-doc-position 'bottom)
                               (setq lsp-ui-doc-alignment 'window))))
  :commands lsp-ui-mode)

;; elisp specific configuration
(add-hook 'emacs-lisp-mode-hook 'electric-pair-local-mode)

;; C/C++ specific configuration
(use-package ccls)
(setq ccls-executable "/usr/bin/ccls")
(setq c-basic-offset 4
      c-default-style "stroustrup")

(defun c-mode-hook-additions ()
  (electric-pair-local-mode)
  (lsp))

(add-hook 'c-mode-hook 'c-mode-hook-additions)
(add-hook 'c++-mode-hook 'c-mode-hook-additions)

;; Python specific configuration
(defun python-mode-hook-additions ()
  (electric-pair-local-mode)
  (lsp))

(add-hook 'python-mode-hook 'python-mode-hook-additions)

;; LSP keybinds
(general-create-definer language-bindings
  :prefix "SPC l"
  :states '(normal emacs)
  :keymaps 'override)

(language-bindings
  ;; Python bindings
  "p r" 'run-python
  "p m" 'python-mode
  "p s b" 'python-shell-send-buffer
  "p s r" 'python-shell-send-region)

(help-bindings
  "d" 'lsp-ui-doc-mode)
