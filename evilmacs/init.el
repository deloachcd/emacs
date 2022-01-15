;; Ensure our package archives are up-to-date and load the
;; package manager
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
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
(setq use-package-compute-statistics t)

;; I mainly use this to keep autosave files contained in user-emacs-directory
(use-package no-littering
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  ;; might as well throw customize bullshit in etc/, since it's there
  (setq custom-file (no-littering-expand-etc-file-name "customize-bullshit.el")))

;; Ensure emacs sees locally-installed binaries in the user's
;; home directory, by adding the local bin to exec-path
(setq user-local-bin (expand-file-name ".local/bin" (getenv "HOME")))
(if (not (member user-local-bin exec-path))
    (setq exec-path (cons user-local-bin exec-path)))

;; custom variables for key paths
(setq user-layers-path (expand-file-name "init-layers" user-emacs-directory))
(setq autoload-path (expand-file-name "extensions" user-emacs-directory))

;; The bulk of our configuration happens in these files
(add-to-list 'load-path user-layers-path)
(add-to-list 'load-path (expand-file-name "languages" user-layers-path))

;; These should be available to init-layers
(add-to-list 'load-path autoload-path)

;; This allows loading local config layers from a list
(defun load-config-layers (layers)
  (defun get-layer-path (layer-name)
    (concat (symbol-name layer-name) "-layer.el"))
  (dolist (layer layers)
    (load (get-layer-path layer))))

;; The bulk of configuration happens in these layers
(setq init-config-layers '(;; theming and font scaling
                           aesthetics
                           ;; which-key and general.el
                           general-keybinds
                           ;; electric pairs/indent ripped off from DOOM emacs
                           electric
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
                           glsl
                           yaml
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
  (put sym 'disabled
       "This emacs doesn't support `customize', configure Emacs from `user-emacs-directory' instead"))
(put 'customize-themes 'disabled "Not supported, use `load-theme' instead")
