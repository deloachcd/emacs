;;; package --- init-evil-org.el
;;;
;;; Commentary:
;;; This is my file for configuring evil-org-mode's behavior.
;;; Note that it is loaded after org mode itself in init.el,
;;; via use-package.
;;;
;;; Code:
(require 'evil-org)

(add-hook 'org-mode-hook 'evil-org-mode)
(add-hook 'evil-org-mode-hook
          (lambda ()
            (evil-org-set-key-theme)))
(require 'evil-org-agenda)
(evil-org-agenda-set-keys)

(provide 'init-evil-org)
;;; init-evil-org ends here
