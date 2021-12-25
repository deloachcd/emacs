;; This layer is for parts of the aesthetic configuration of this
;; emacs which leverage packages pulled from external sources
;; after package refresh.
(provide 'aesthetics-layer)

(use-package dashboard
  :init
  ;;;(setq dashboard-startup-banner (concat user-emacs-directory "/res/img/toothpaste-desat.png"))
  ;;(setq dashboard-center-content t)
  ;;(setq dashboard-set-footer nil)
  (setq dashboard-startup-banner 1)
  (setq dashboard-items '((recents . 5)
                          (projects. 5)))
  :config
  (dashboard-setup-startup-hook))

(use-package doom-modeline
  :init
  (setq doom-modeline-height 30)
  (setq doom-modeline-modal-icon nil)
  :config (doom-modeline-mode 1))

(use-package all-the-icons
  :config
  (let ((has-fonts (= (shell-command
					   "test -e ~/.local/share/fonts/all-the-icons.ttf")
					  0)))
	(unless has-fonts
	  (all-the-icons-install-fonts))))

(use-package rainbow-delimiters
  :hook (prog-mode-hook . rainbow-delimiters-mode))
