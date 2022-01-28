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

  ;; modus operandi gets its own theming for git-gutter-fr, because it's built-in
  ;; to emacs and thus it makes sense to theme it here
  (if (string= (car custom-enabled-themes) "modus-operandi")
      (progn
        (fringe-helper-define 'git-gutter-fr:added nil
          ".......x"
          ".......x"
          ".......x"
          ".......x"
          ".......x"
          ".......x"
          ".......x"
          ".......x")
        (fringe-helper-define 'git-gutter-fr:modified nil
          ".......x"
          ".......x"
          ".......x"
          ".......x"
          ".......x"
          ".......x"
          ".......x"
          ".......x")
        
        (set-face-attribute 'git-gutter-fr:added nil :background "#9bc99e"
                            :foreground (face-attribute 'default :background))
        (set-face-attribute 'git-gutter-fr:modified nil :background "#9bcfd9"
                            :foreground (face-attribute 'default :background)))
    ;; default behavior - thin fringe bitmaps lifted from doom emacs
    (progn
      (define-fringe-bitmap 'git-gutter-fr:added [224]
        nil nil '(center repeated)
      (define-fringe-bitmap 'git-gutter-fr:modified [224]
        nil nil '(center repeated)))))

  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
    nil nil 'bottom)

  ;; git-gutter-fringe in prog-mode buffers
  (add-hook 'prog-mode-hook 'git-gutter-mode))
