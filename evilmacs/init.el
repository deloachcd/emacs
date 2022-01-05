;; Disable annoying warnings in self-compiled emacs
;;(setq warning-minimum-level :emergency)

;; Ensure emacs sees locally-installed binaries in the user's
;; home directory, by adding the local bin to exec-path
(setq user-local-bin (concat (getenv "HOME") "/.local/bin"))
(setq exec-path (cons user-local-bin exec-path))

;; custom variables for some useful paths
(setq user-layers-path (concat user-emacs-directory "init-layers"))
(setq autoload-path (concat user-emacs-directory "autoload"))

;; The bulk of our configuration happens in these files
(add-to-list 'load-path user-layers-path)
(add-to-list 'load-path (concat user-layers-path "/languages"))

;; These should be available to init-layers
(add-to-list 'load-path autoload-path)

;; This allows loading local config layers from a list
(defun load-config-layers (layers)
  (defun get-layer-path (layer-name)
    (concat (symbol-name layer-name) "-layer.el"))
  (dolist (layer layers)
    (load (get-layer-path layer))))

;; Ensure our package archives are up-to-date and load the
;; package manager
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("elpa" . "https://elpa.gnu.org/packages/")))
(require 'package)
(package-initialize)

;; Load use-package and make sure each entry is downloaded
(unless (and (fboundp 'package-installed-p) 
             (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; The bulk of configuration happens in these layers
(setq init-config-layers '(;; theming and font scaling happens first
                           aesthetics
                           ;; next, which-key and general.el are made available
                           general-keybinds
                           ;; general editor configuration happens here
                           editing
                           ;; these layers target specific (sets of) extension(s)
                           completion
                           evil-god
                           helpful
                           projectile
                           flycheck
                           org
                           git
                           shell
                           ;; language-specific layers
                           emacs-lisp
                           python
                           c
                           ))

(load-config-layers init-config-layers)

;; I lifted some code from DOOM emacs to disable customize, because they're
;; right on the money about `customize' being dogshit. 
;;
;; All configuration should be done from this file or one of its layers.
(dolist (sym '(customize-option customize-browse customize-group customize-face
                                customize-rogue customize-saved customize-apropos
                                customize-changed customize-unsaved customize-variable
                                customize-set-value customize-customized customize-set-variable
                                customize-apropos-faces customize-save-variable
                                customize-apropos-groups customize-apropos-options
                                customize-changed-options customize-save-customized))
  (put sym 'disabled "This emacs doesn't support `customize', configure Emacs from ~/.emacs.d/ instead"))
(put 'customize-themes 'disabled "Not supported, use `load-theme' instead")

;; Send "custom-set-variables" output to hell
(setq custom-file null-device)
