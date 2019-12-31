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
		     projectile))

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

;; make sure nothing can override evil keybinds
(setq evil-overriding-maps nil
      evil-intercept-maps nil)

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
  :states '(normal emacs)
  :keymaps 'override)
(globals
 "SPC" 'execute-extended-command)

;; help keybindings
(general-create-definer help
  :prefix "SPC h"
  :states '(normal emacs)
  :keymaps 'override)
(help
  "k" 'describe-key)

;; window management keybindings
(general-create-definer windows
  :prefix "SPC w"
  :states '(normal emacs)
  :keymaps 'override)
(windows
  "/" 'split-window-right
  "-" 'split-window-below
  "h" 'windmove-left
  "j" 'windmove-down
  "k" 'windmove-up
  "l" 'windmove-right)

;; buffers
(general-create-definer buffers
  :prefix "SPC b"
  :states '(normal emacs)
  :keymaps 'override)
(buffers
 "b" 'ivy-switch-buffer
 "n" 'next-buffer
 "p" 'previous-buffer
 "k" 'kill-buffer)

(general-create-definer elisp-eval
  :prefix "SPC e"
  :states '(normal emacs visual)
  :keymaps 'override)
(elisp-eval
 "b" 'eval-buffer
 "l" 'eval-last-sexp
 "r" 'eval-region)

(defun dotfile-reload ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(general-create-definer files
  :prefix "SPC f"
  :states '(normal emacs)
  :keymaps 'override)
(files
  "eR" 'dotfile-reload)

;; projectile
(setq projectile-project-search-path
      '("~/.emacs.d"
	"~/Projects"))

(general-create-definer projectile
  :prefix "SPC p"
  :states '(normal emacs)
  :keymaps 'override)
(projectile
 "f" 'projectile-find-file
 "p" 'projectile-switch-project)


;; TODO: general-evil-definer
;; TODO: clojure / cider
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
