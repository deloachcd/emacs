;; This layer is for parts of the aesthetic configuration of this
;; emacs which leverage packages pulled from external sources
;; after package refresh.
(provide 'aesthetics-layer)

(setq inhibit-startup-message t)
(setq initial-scratch-message "\
;;
;;   ╻╻ ╻ ╻   ╻ ╻┏━╸╻  ┏━╸┏━┓┏┳┓┏━╸   ╺┳╸┏━┓   ┏━╸┏┳┓┏━┓┏━╸┏━┓    ╻╻ ╻ ╻
;;  ┏┛╺╋╸┏┛   ┃╻┃┣╸ ┃  ┃  ┃ ┃┃┃┃┣╸     ┃ ┃ ┃   ┣╸ ┃┃┃┣━┫┃  ┗━┓   ┏┛╺╋╸┏┛
;;  ╹ ╹ ╹╹    ┗┻┛┗━╸┗━╸┗━╸┗━┛╹ ╹┗━╸    ╹ ┗━┛   ┗━╸╹ ╹╹ ╹┗━╸┗━┛   ╹ ╹ ╹╹
;;
;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

")
;;(setq initial-major-mode 'python-mode)

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
