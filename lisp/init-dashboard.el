;;; package --- init-dashboard
;;;
;;; Commentary:
;;; Set up configuration for dashboard
;;; 
;;; Code:

(defun get-nth-element-in-list (listarg n)
  "LISTARG is a list, and N assumes the list is zero-indexed."
  (defun inner-recursion (list cdrs-remaining)
    (if (eq cdrs-remaining 0) (car list) (inner-recursion (cdr list)
                                                          (- cdrs-remaining 1))))
  (inner-recursion listarg n))

(defun get-random-element-in-list (listarg)
  "LISTARG is a list, as one would hope it would be!"
  (get-nth-element-in-list listarg (random (length listarg))))

(defvar banner-combos '(("iTunes -- The world's favorite music player!"
                         "~/.emacs.d/images/itunes.png")
                        ("Vi IMproved -- Help poor children in Uganda!"
                         "~/.emacs.d/images/vim.png")
                        ("FreeBSD -- It's better than Linux, I swear!"
                         "~/.emacs.d/images/freebsd.png")
                        ("Python -- The language of choice for babies and neckbeards alike!"
                         "~/.emacs.d/images/python.png")
                        ("Ada -- The future of programming!"
                         "~/.emacs.d/images/ada.png")
                        ("Windows Vista -- The world's most loved operating system!"
                         "~/.emacs.d/images/vista.png")
                        ("Visual Studio Code -- The noncontrarian developer's choice!"
                         "~/.emacs.d/images/vscode.png")
                        ("Internet Explorer -- Remember to call your grandparents every now and then!"
                         "~/.emacs.d/images/ie.png")
                        ("Linux -- sudo rm -rf / --no-preserve-root"
                         "~/.emacs.d/images/linux.png")))

;; Use this to test new banner combos
;; (defvar selected-banner-combo (get-nth-element-in-list banner-combos <COMBO_NUM>))

(defvar selected-banner-combo (get-random-element-in-list banner-combos))
(defvar dashboard-banner-logo-title (car selected-banner-combo))
(defvar dashboard-startup-banner (cadr selected-banner-combo))
(defvar dashboard-center-content t)

(provide 'init-dashboard)
;;; init-dashboard ends here
