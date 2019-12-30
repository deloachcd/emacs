;; Temporary fix for 'undo-tree' signature issue upstream
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(require 'package)

;; List packages for install
(setq package-list '(evil
                     evil-leader
		     general))

;; Add Melpa as the default Emacs Package repository
;; only contains a very limited number of packages
(setq package-archives
  '(("gnu" . "https://elpa.gnu.org/packages/")
    ("melpa" . "https://melpa.org/packages/")
    ("melpa-stable" . "https://stable.melpa.org/packages/")))

;; Activate all the packages (in particular autoloads)
(package-initialize)

;; Update your local package index
;;(package-refresh-contents)
(unless package-archive-contents
  (package-refresh-contents))

;; Install all missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; evil mode
(require 'evil)
(require 'evil-leader)
(global-evil-leader-mode)
(evil-mode t)

;; custom themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'badger t)

;; global keybindings
(general-create-definer globals
  :prefix "SPC"
  :states 'normal
  :keymaps 'override)
(globals
 "SPC" 'execute-extended-command)

;; help keybindings
(general-create-definer help
  :prefix "SPC h"
  :states 'normal
  :keymaps 'override)
(help
  "k" 'describe-key)

;; Window management keybindings
(general-create-definer windows
  :prefix "SPC w"
  :states 'normal
  :keymaps 'override)
(windows
  "/" 'split-window-right
  "-" 'split-window-below
  "h" 'windmove-left
  "j" 'windmove-down
  "k" 'windmove-up
  "l" 'windmove-right)

;; TODO: general-evil-definer
;; TODO: files
;; TODO: buffers
;; TODO: clojure / cider
;; TODO: projectile
;; TODO: ivy integration
;; TODO: counsel

;; | {}{}{}{}{}{}{}{}{}{}{}{}{}{} |
;; | no manual editing below here |
;; | {}{}{}{}{}{}{}{}{}{}{}{}{}{} |
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (evil-leader evil undo-tree))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
