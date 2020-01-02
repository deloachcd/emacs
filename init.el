;; Temporary fix for 'undo-tree' signature issue upstream
(if (string= emacs-version "26.1")
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(require 'package)

;; List packages for install
(setq package-list '(undo-tree
                     evil
                     evil-leader
                     general
                     ivy
		     counsel
                     projectile))

;; Add Melpa as the default Emacs Package repository
;; only contains a very limited number of packages
(setq package-archives
  '(("gnu" . "https://elpa.gnu.org/packages/")
    ("melpa" . "https://melpa.org/packages/")
    ("melpa-stable" . "https://stable.melpa.org/packages/")))

;; Update your local package index
(unless package-archive-contents
  (package-refresh-contents))

;; Activate all the packages (in particular autoloads)
(package-initialize)

;; Install all missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; add paths to our custom stuff in .emacs.d
(add-to-list 'load-path "~/.emacs.d/init-layers")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; user config settings
(load-theme 'badger t)
(counsel-mode 1)
(menu-bar-mode -1)

;; configuration layers
(require 'evil-layer)
(require 'global-layer)
(require 'help-layer)
(require 'window-layer)
(require 'buffer-layer)
(require 'eval-layer) ; arbitrary language evaluation
(require 'file-layer)
(require 'projectile-layer)

;; TODO: clojure / cider
;; TODO: organize configuration into layers

;; | {}{}{}{}{}{}{}{}{}{}{}{}{}{} |
;; | no manual editing below here |
;; | {}{}{}{}{}{}{}{}{}{}{}{}{}{} |
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (counsel evil-leader evil undo-tree))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
