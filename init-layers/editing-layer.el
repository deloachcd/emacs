;; Configuration related to the general behavior of emacs as a
;; text editor for both text files and source code files, making
;; heavy use of evil-mode because I can't live in a text editor
;; without vim bindings.
;;
;; Much of the ideas here are taken from Witchmacs, by snackon:
;; https://github.com/snackon/Witchmacs

;; start directly to scratch buffer
(setq inhibit-startup-screen t)

;; Scroll one line at a time, like it should be
(setq scroll-conservatively 100)

;; Indentation

;; Prefer spaces over tabs, standard width of 4, because not every editor is
;; smart enough to render tabs like this out of the box.
(setq-default tab-width 4
              standard-indent 4
              indent-tabs-mode nil)

;; Only indent the line when at BOL or in a line's indentation. Anywhere else,
;; insert literal indentation.
(setq-default tab-always-indent nil)

;; Make `tabify' and `untabify' only affect indentation. Not tabs/spaces in the
;; middle of a line.
(setq tabify-regexp "^\t* [ \t]+")

;; Show mark for indentation via tabs, as they can be
;; inconsistent
(setq whitespace-style '(tab-mark))

;; Don't litter every working directory with backups
(defvar backup-dir (expand-file-name "backups" user-emacs-directory))
(setq backup-directory-alist (list (cons "." backup-dir)))

;; Copypasting outside of emacs
(setq x-select-enable-clipboard t)

;; Disable ring-bell
(setq ring-bell-function 'ignore)

;; Type "y" or "n" instead of "yes" or "no" in minibuffer
(defalias 'yes-or-no-p 'y-or-n-p)

;; Always just jump to the real file, don't ask
(setq vc-follow-symlinks t)

;; ensure line number width doesn't change as we scroll down
(setq display-line-numbers-width-start t)

;; set fill-column to 80 in accordance with the sacred 80 column rule
(setq fill-column 80)
