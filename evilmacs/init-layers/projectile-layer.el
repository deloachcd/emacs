;; Configuration related to project management with projectile.
(require 'general-keybinds-layer)

(use-package projectile
  :defer
  :init (setq projectile-project-search-path '("~"
                                               "~/Projects"
                                               "~/Sync/Documents/org"))
  :config (projectile-mode 1))

;; Use projectile for packages to find project root,
;; rather than project.el
(defun projectile-project-find-function (dir)
  (let* ((root (projectile-project-root dir)))
    (and root (cons 'transient root))))

;; Use projectile-project-root to run build script from keybinding
(defun run-project-from-entrypoint ()
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (if (and (= (shell-command "test -e builders/make-debug.sh") 0)
             (= (shell-command "test -e builders/make-run.sh") 0))
        (shell-command "./builders/make-run.sh ./builders/make-debug.sh &")
      (message "Cannot find project runner and/or make script!"))))

(with-eval-after-load 'project
  (add-to-list 'project-find-functions 'projectile-project-find-function))

(general-create-definer projectile-bindings
  :prefix "SPC p"
  :states '(normal emacs)
  :keymaps 'override)
(projectile-bindings
  "" '(nil :which-key "project")
  "f" 'projectile-find-file
  "p" 'projectile-switch-project
  "r" 'run-project-from-entrypoint)
