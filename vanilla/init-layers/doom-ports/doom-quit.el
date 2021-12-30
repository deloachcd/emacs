;;; ui/doom-quit/config.el -*- lexical-binding: t; -*-

(defvar +doom-quit-messages
  '(;; from Doom 1
    "Please don't leave, there's more demons to toast!"
    "Let's beat it -- This is turning into a bloodbath!"
    "I wouldn't leave if I were you. UNIX is much worse."
    "Don't leave yet -- There's a demon around that corner!"
    "Ya know, next time you come in here I'm gonna toast ya."
    "Go ahead and leave. See if I care."
    "Are you sure you want to quit this great editor?"
    ;; from Doom 2
    "Don't go now, there's a dimensional shambler waiting at the shell prompt!"
    "Get outta here and go back to your boring programs."
    "If I were your boss, I'd deathmatch ya in a minute!"
    "Look, bud. You leave now and you forfeit your body count!"
    "You're lucky I don't smack you for thinking about leaving.")
  "A list of quit messages, picked randomly by `+doom-quit'. Taken from
http://doom.wikia.com/wiki/Quit_messages and elsewhere.")

(defun +doom-quit-fn (&rest _)
  (doom-quit-p
   (format "%s  %s"
           (propertize (nth (random (length +doom-quit-messages))
                            +doom-quit-messages)
                       'face '(italic default))
           "Really quit Emacs?")))

;;
(setq confirm-kill-emacs #'+doom-quit-fn)
