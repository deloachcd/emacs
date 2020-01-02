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
(add-to-list 'load-path "~/.emacs.d/init-layers") ; config layers
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes") ; custom themes

;; config layers
(require 'evil-layer)
(require 'general-layer)    ; SPC
(require 'help-layer)       ; SPC-h
(require 'window-layer)     ; SPC-w
(require 'buffer-layer)     ; SPC-b
(require 'eval-layer)       ; SPC-e
(require 'file-layer)       ; SPC-f
(require 'projectile-layer) ; SPC-p

;; TODO: clojure / cider

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
