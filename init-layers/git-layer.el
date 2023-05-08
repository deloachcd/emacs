;; Configuration related to git integration
(require 'general-keybinds-layer)

(use-package magit
  :defer)

(general-create-definer magit-bindings
  :prefix "SPC g"
  :states '(normal emacs)
  :keymaps 'override)

(magit-bindings
  "" '(nil :which-key "git")
  "g" 'magit-status)

(use-package git-gutter-fringe
  ;;:hook (prog-mode . git-gutter-mode)
  :init
  (setq git-gutter:disabled-modes '(fundamental-mode image-mode pdf-view-mode))
  :config
  ;; ensure git-gutter picks up changes from magit / external git when
  ;; we pop back into the window
  (add-hook 'focus-in-hook #'git-gutter:update-all-windows)

  ;; places the git gutter outside the margins.
  (setq-default fringes-outside-margins t)

  (define-fringe-bitmap 'git-gutter-fr:added [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
    nil nil 'bottom)

  ;; git-gutter-fringe in prog-mode buffers
  (add-hook 'prog-mode-hook 'git-gutter-mode))
