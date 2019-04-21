;;; .emacs --- the init file
;;;
;;; Commentary:
;;; This is my .emacs config.
;;; Didn't want to use 'spacemacs' or 'doom-emacs' or some shit like
;;; that, because why not just use VSCode at that point.
;;;
;;; Code:

;; Add directory for 'require' loading
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Make sure to load the package manager - package.el
(require 'package)

;; Add popular community sources to package manager list
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

;; Enable the package manager
(setq package-enable-at-startup nil)
(package-initialize)

;; Automatically install use-package if we don't already have it
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package evil
  :ensure t
  :config
  (require 'init-evil))

(use-package helm
  :ensure t
  :config
  (require 'init-helm))

(use-package org
  :ensure t
  :config
  (require 'init-org))

(use-package evil-org
  :ensure t
  :after org
  :config
  (require 'init-evil-org))

(use-package auto-complete
  :ensure t
  :config
  (ac-config-default))

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))

(use-package flycheck
  :ensure t
  :config
  (require 'init-flycheck))

(use-package better-defaults
  :ensure t
  :config
  (require 'better-defaults))

(use-package spaceline
  :ensure t
  :config
  (spaceline-spacemacs-theme))

(use-package page-break-lines
  :ensure t
  :config
  (global-page-break-lines-mode))

(use-package dashboard
  :ensure t
  :init
  (require 'init-dashboard)
  :config
  (dashboard-setup-startup-hook))

(use-package magit
  :ensure t)

(use-package evil-magit
  :ensure t
  :config
  (requre 'evil-magit))

;; Set the default font to a nice, readable one
(defvar default-fixed-width "Source Code Variable 14")
(set-frame-font default-fixed-width nil t)

;; Enable functionality I like that emacs provides out of the box

;; ido-mode for switching buffers
;; (require 'ido)
;; (ido-mode t)

;; windmove + keybinds
(require 'init-windmove)

;; electric-pair-mode for auto-closures
(electric-pair-mode)

;; smoothen up the scrolling
(setq scroll-step 1)

;; behavior of tabs key
(progn
  ;; make indentation commands use space only (never tab character)
  (setq-default indent-tabs-mode nil))
(setq-default tab-width 4)


(provide '.emacs)
;;; .emacs ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("e7b49145d311e86da34a32a7e1f73497fa365110a813d2ecd8105eaa551969da" default)))
 '(package-selected-packages
   (quote
    (evil-magit magit spaceline zeno-theme zenburn-theme yasnippet use-package twilight-theme spacemacs-theme smart-mode-line s pyvenv nlinum-relative molokai-theme moe-theme highlight-indentation helm gruvbox-theme flycheck flatland-theme find-file-in-project evil doom-themes darktooth-theme better-defaults badger-theme auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
