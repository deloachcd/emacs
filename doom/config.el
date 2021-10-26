;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Add my own stuff to load-path
;;(add-load-path! (concat doom-private-dir "/autoload"))

;; Disable GUI toolbars
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

;; Slight padding for content in frame
(set-fringe-mode 10)

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Chandler DeLoach"
      user-mail-address "deloach.cdd@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(defun get-font-size-from-resolution ()
  (progn
    (defun get-display-resolution-windows ()
      "TODO: actually implement this"
      "1920x1080")
    (defun get-display-resolution-linux ()
      (shell-command-to-string "xrandr | grep '*' | head -n 1 | awk '{ printf $1 }'"))

    (let ((resolution (cond ((string-equal system-type "gnu/linux")
                             (get-display-resolution-linux))
                            ((string-equal system-type "windows-nt")
                             (get-display-resolution-windows)))))

      (cond (;; 4k
             (string-equal resolution "3840x2160") 20)

            ;; 1080p
            ((string-equal resolution "1920x1080") 20)

            ;; Default case - same as 1080p for now
            (t  20)))))

(setq doom-font (font-spec :family "Fira Code" 
                           :size (get-font-size-from-resolution))
      doom-variable-pitch-font (font-spec :family "Noto Sans"
                                          :size (+ (get-font-size-from-resolution) 2)))

(defun set-frame-defaults (frame-width frame-height)
  (if (display-graphic-p)
      (let ((frame-size-params '((width . frame-width) (height . frame-height))))
        (setq initial-frame-alist frame-size-params)
        (setq default-frame-alist frame-size-params)
        (when window-system (set-frame-size (selected-frame) frame-width frame-height)))))

(setq default-frame-alist
      (append (list
	       '(height         . 42)
	       '(width          . 108)
               '(left-fringe    . 0)
               '(right-fringe   . 0))))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'dehydrated)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Sync/Documents/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; org-mode setup
(after! org
  ;; set up fonts in org-mode
  (progn
    ;; intelligently mix monospace and variable-pitch fonts in org-mode
    (use-package! mixed-pitch
      :hook (org-mode . mixed-pitch-mode))
    ;; Give headings larger, bold font
    (dolist (face '((org-document-title . 1.0)
                    (org-level-1 . 1.5)
                    (org-level-2 . 1.25)
                    (org-level-3 . 1.1)
                    (org-level-4 . 1.0)
                    (org-level-5 . 1.0)
                    (org-level-6 . 1.0)
                    (org-level-7 . 1.0)
                    (org-level-8 . 1.0)))
      (set-face-attribute (car face) nil
                          :inherit 'doom-variable-pitch-font :weight 'bold :height (cdr face)))
    ;; don't show the org-roam backlinks buffer whenever we load any roam file
    (setq +org-roam-open-buffer-on-find-file nil)))
