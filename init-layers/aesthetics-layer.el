;; This layer is for parts of the aesthetic configuration of this
;; emacs which leverage packages pulled from external sources
;; after package refresh.
(provide 'aesthetics-layer)

(use-package dashboard
  :init
  (setq dashboard-startup-banner "~/.emacs.d/res/img/zenmacs.png")
  (setq dashboard-center-content t)
  (setq dashboard-set-footer nil)
  (setq dashboard-items '((recents . 5)
                          (projects . 5)))
  :config
  (dashboard-setup-startup-hook))

(use-package doom-modeline
  :init
  (setq doom-modeline-icon nil)
  (setq doom-modeline-height 30)
  :config (doom-modeline-mode 1))
