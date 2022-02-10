;;; os/tty/config.el -*- lexical-binding: t; -*-

;; Keep window title up-to-date. Should fail gracefully in non-xterm terminals.
;; Only works in Emacs 27+.
(setq xterm-set-window-title t)
;; DEPRECATED Not needed on Emacs 28+. Remove when dropping 27 support.
(defadvice! +tty--only-set-window-title-in-tty-a (&optional terminal)
  "`xterm-set-window-title' fails in GUI Emacs. Stop that. Get some help."
  :before-while #'xterm-set-window-title
  (not (display-graphic-p terminal)))

;; Some terminals offer two different cursors: a "visible" static cursor and a
;; "very visible" blinking one. By default, Emacs uses the very visible cursor
;; and will switch back to it when Emacs is started or resumed. A nil
;; `visible-cursor' prevents this.
(setq visible-cursor nil)

;; Enable the mouse in terminal Emacs
(add-hook 'tty-setup-hook #'xterm-mouse-mode)

;; Windows terminals don't support what I'm about to do, but best not to wrap
;; this in a IS-WINDOWS check, in case you're using WSL or Cygwin, which do and
;; *might* support it.
(add-hook! 'tty-setup-hook
  (defun doom-init-clipboard-in-tty-emacs-h ()
    ;; Fix the clipboard in tty Emacs by...
    (if (featurep! +osc)
        ;; ...communicating with the clibpoard through OSC escape codes (must
        ;; use a terminal that supports it)
        (and (require 'clipetty nil t)
             (global-clipetty-mode +1))
      ;; ...OR piping clipboard I/O through xclip, xsel, pb{copy,paste},
      ;; wl-copy, termux-clipboard-get, or getclip (cygwin); depending on what
      ;; is available.
      (and (require 'xclip nil t)
           (with-demoted-errors "%s" (xclip-mode +1))))))

(when (featurep! :editor evil)
  ;; Fix cursor shape-changing in the terminal. Only supported in XTerm, Gnome
  ;; Terminal, iTerm, Konsole, dumb (etc. mintty), and Apple Terminal.app. If
  ;; using Apple Terminal.app, install
  ;; http://www.culater.net/software/SIMBL/SIMBL.php and
  ;; https://github.com/saitoha/mouseterm-plus/releases. That makes to support
  ;; VT's DECSCUSR sequence.
  (add-hook 'tty-setup-hook #'evil-terminal-cursor-changer-activate))
