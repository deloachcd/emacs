;; Ensure emacs sees locally-installed binaries in the user's
;; home directory, by adding the local bin to exec-path
(setq user-local-bin (concat (getenv "HOME") "/.local/bin"))
(setq exec-path (cons user-local-bin exec-path))

;; The bulk of our configuration happens in these files
(add-to-list 'load-path "~/.emacs.d/init-layers")

;; These should be available to init-layers
(add-to-list 'load-path "~/.emacs.d/autoload")

;; This allows loading local config layers from a list
(defun load-config-layers (layers)
  (defun get-layer-path (layer-name)
    (concat (symbol-name layer-name) "-layer.el"))
  (dolist (layer layers)
    (load (get-layer-path layer))))

;; This layer is loaded before package refresh, because
;; it can take a bit and this looks much nicer than
;; waiting for theming to load
(setq early-init-config-layers '(early-init-aesthetics))
(load-config-layers early-init-config-layers)

;; Ensure our package archives are up-to-date and load the
;; package manager
(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("elpa" . "https://elpa.gnu.org/packages/")))
(unless package-archive-contents
  (package-refresh-contents))
(package-initialize)

;; Load use-package and make sure each entry is downloaded
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; The bulk of configuration happens in these layers
(setq init-config-layers '(aesthetics
                           editing
                           minibuffer-completion
                           general-keybinds
                           ide
                           projectile
                           org
                           magit))

(load-config-layers init-config-layers)

;; I lifted some code from DOOM emacs to disable customize, because they're
;; right on the money about `customize' being "a clumsy interface that
;; sets variables at a time where it can be easily and unpredictably
;; overwritten."
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
