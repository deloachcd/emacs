;; This does what DOOM does to disable the garbage collector
;; during init to improve performance, and restore it to
;; sensible default values once init is done
(setq startup/gc-cons-threshold gc-cons-threshold)
(setq startup/gc-cons-percentage gc-cons-percentage)

(setq gc-cons-threshold most-positive-fixnum
	  gc-cons-percentage 0.6)

(defun startup/reset-gc ()
  (setq gc-cons-threshold startup/gc-cons-threshold)
  (setq gc-cons-percentage startup/gc-cons-percentage))

(add-hook 'emacs-startup-hook 'startup/reset-gc)
