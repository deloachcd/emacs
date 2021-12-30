(use-package git-gutter-fringe
  :config
  (setq git-gutter:disabled-modes '(fundamental-mode image-mode pdf-view-mode))
  :init
  (progn
	;; ensure git-gutter picks up changes from magit / external git
	(add-hook 'focus-in-hook #'git-gutter:update-all-windows)

	;; TODO: port this!
	;;(advice-add #'magit-stage-file   :after #'+vc-gutter-update-h)
	;;(advice-add #'magit-unstage-file :after #'+vc-gutter-update-h)

	;; git-gutter-fringe aesthetic config lifted right from DOOM emacs
    (if (fboundp 'fringe-mode) (fringe-mode '4))
    ;; places the git gutter outside the margins.
    (setq-default fringes-outside-margins t)
    ;; thin fringe bitmaps
    (define-fringe-bitmap 'git-gutter-fr:added [224]
      nil nil '(center repeated))
    (define-fringe-bitmap 'git-gutter-fr:modified [224]
      nil nil '(center repeated))
    (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
      nil nil 'bottom)

	(add-hook 'prog-mode-hook 'git-gutter-mode)))
