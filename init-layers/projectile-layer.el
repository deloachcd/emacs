(provide 'projectile-layer)

(setq projectile-project-search-path '("~")
      projectile-completion-system 'ivy)

(general-create-definer projectile
  :prefix "SPC p"
  :states '(normal emacs)
  :keymaps 'override)
(projectile
 "f" 'projectile-find-file
 "p" 'projectile-switch-project)

(projectile-mode 1)
