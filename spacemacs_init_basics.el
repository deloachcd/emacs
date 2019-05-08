(defun dotspacemacs/init ()
  (setq-default
   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-themes '(dorsey
                         mccarthy)
   dotspacemacs-default-font '("Source Code Pro"
                               :size 15
                               :weight normal
                               :width normal)
   dotspacemacs-line-numbers 'relative

   ))

(defun dotspacemacs/user-config ()
  (with-eval-after-load 'org
    (setq org-agenda-files '("~/Documents/org/micfo.org")))
  (setq projectile-project-search-path '("~/Projects" "~/Documents/org"))
  (push (cons "\\*shell\\*" display-buffer--same-window-action) display-buffer-alist)
)
