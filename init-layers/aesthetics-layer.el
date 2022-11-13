(defvar display-is-hidpi nil
  "true when emacs is running in a graphical session on a HiDPI display")

(defun get-display-resolution-linux ()
  (shell-command-to-string "xrandr | grep '*' | head -n 1 | awk '{ printf $1 }'"))

(when (and (string= system-type "gnu/linux")
         (string= (get-display-resolution-linux) "3840x2160"))
    (setq display-is-hidpi t))

(defvar global-fringe-width 5
  "width for fringes on side of window")

;; extra pixel for HiDPI display
(when display-is-hidpi
  (setq global-fringe-width 6))

;; Slight padding for content in frame
(set-fringe-mode global-fringe-width)

;; Load themes from ~/.emacs.d/themes
(add-to-list 'custom-theme-load-path
             (expand-file-name "themes" user-emacs-directory))
(use-package modus-themes
  :init
  ;; modus theme configuration
  ;; -------------------------
  ;; general options
  (setq modus-themes-bold-constructs t)
  (setq modus-themes-italic-constructs t)
  (setq modus-themes-syntax '(faint))
  ;; org-mode
  (setq modus-themes-org-blocks 'gray-background)
  (setq modus-themes-markup '(background intense))
  (setq modus-themes-headings
        '((1 . (1.1))
          (2 . (1.05))
          (3 . (1.0))
          (4 . (1.0))))
  (setq modus-themes-mixed-fonts t)
  :config
  (load-theme 'modus-operandi t))

(defun size-and-apply-fonts (fixed-pitch-font variable-pitch-font fixed-size variable-size)
  "Sets the and sizes the fixed and variable pitch fonts for current and new frames, accounting for whether or not the running display is HiDPI."
    (defun set-fonts-from-heights (fixed-pitch-height variable-pitch-height)
      (set-face-attribute 'default nil
                          :font fixed-pitch-font :height fixed-pitch-height)
      (set-face-attribute 'fixed-pitch nil
                          :font fixed-pitch-font :height fixed-pitch-height)
      (set-face-attribute 'variable-pitch nil
                          :font variable-pitch-font :height variable-pitch-height)
      (add-to-list 'default-frame-alist (cons 'font 'default))
      (set-frame-font 'default nil t))

    (let ((bump-factor (if display-is-hidpi 15 0)))
      (set-fonts-from-heights (+ fixed-size bump-factor)
                              (+ variable-size bump-factor))))

;; This function sets default params for a frame, and resizes the current frame
;; to that size
(defun set-frame-defaults (frame-width frame-height)
  (if (display-graphic-p)
      (let ((frame-size-params '((width . frame-width) (height . frame-height))))
        (setq initial-frame-alist frame-size-params)
        (setq default-frame-alist frame-size-params)
        (when window-system (set-frame-size (selected-frame) frame-width frame-height)))))

;; We call our functions to apply their changes here
(if (string-equal system-type "darwin")
    (size-and-apply-fonts "Menlo" "Helvetica" 140 150)
  (size-and-apply-fonts "Ubuntu Mono" "Noto Sans" 130 120))
(set-frame-defaults 88 36)

(use-package mood-line
  :init
  (setq mood-line-show-encoding-information t)
  (setq mood-line-show-eol-style t)
  :config
  (mood-line-mode))

(use-package dashboard
  :init
  (setq dashboard-center-content t)
  (setq dashboard-set-footer nil)
  ;;(setq dashboard-startup-banner 1)
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents . 5)
                          (projects . 5))))

;; tab bar aesthetic configuration
(setq tab-bar-new-button-show nil)
(setq tab-bar-close-button-show nil)
(setq tab-bar-button-margin 4)
(defun padded-tab-name-function ()
  "Function that just adds spaces surrounding the name of the current buffer."
  (concat " " (buffer-name (window-buffer (minibuffer-selected-window))) " "))
(setq tab-bar-tab-name-function 'padded-tab-name-function)
