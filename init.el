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

;; user config settings
(setq
 auto-window-vscroll nil)

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
  "k" 'describe-key
  "f" 'describe-function)

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

;; language evaluation
(defun ensure-newline (string)
  "Ensure string ends in a newline."
  (if (string= (substring string -1) "\n")
      string
    (concat string "\n")))

(defun eval-buffer-sh ()
  (interactive)
  (process-send-string "*shell*"
		       (ensure-newline (buffer-string))))

(defun eval-region-sh ()
  (interactive)
  (process-send-string "*shell*"
		       (ensure-newline (buffer-substring
					(region-beginning) (region-end)))))

(defun eval-line-sh ()
  (interactive)
  (process-send-string "*shell*"
		       (ensure-newline (thing-at-point 'line))))

(general-create-definer elisp-eval
  :prefix "SPC e"
  :states '(normal emacs visual)
  :keymaps 'override)
(elisp-eval
  ;; elisp
  "eb" 'eval-buffer
  "el" 'eval-last-sexp
  "er" 'eval-region
  ;; bash
  "bb" 'eval-buffer-sh
  "br" 'eval-region-sh
  "bl" 'eval-line-sh
  )

(defun dotfile-reload ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(general-create-definer files
  :prefix "SPC f"
  :states '(normal emacs)
  :keymaps 'override)
(files
  "f" 'counsel-find-file
  "dR" 'dotfile-reload)

;; projectile
(setq projectile-project-search-path '("~")
      projectile-completion-system 'ivy)
(projectile-mode 1)

(general-create-definer projectile
  :prefix "SPC p"
  :states '(normal emacs)
  :keymaps 'override)
(projectile
 "f" 'projectile-find-file
 "p" 'projectile-switch-project)

;; ivy
(counsel-mode 1)

;; other mode switches
(menu-bar-mode -1)

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
