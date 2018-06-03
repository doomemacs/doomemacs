;;; ui/popup/config.el -*- lexical-binding: t; -*-

(defconst +popup-window-parameters
  '(transient quit select modeline popup)
  "A list of custom parameters to be added to `window-persistent-parameters'.
Modifying this has no effect, unless done before ui/popup loads.

(transient . CDR)
  CDR can be t, an integer, nil or a function that returns one of these. It
  represents the number of seconds before the buffer belonging to a closed popup
  window is killed.

  If t, CDR will default to `+popup-ttl'.
  If 0, the buffer is immediately killed.
  If nil, the buffer won't be killed.
  If a function, it must return one of the other possible values above. It takes
    the popup buffer as its sole argument.

(quit . CDR)
  CDR can be t, 'other, 'current, nil, or a function that returns one of these.
  This determines the behavior of the ESC/C-g keys in or outside of popup
  windows.

  If t, close the popup if ESC/C-g is pressed inside or outside of popups.
  If 'other, close this popup if ESC/C-g is pressed outside of any popup. This
    is great for popups you just want to peek at and discard, but might also
    want to poke around in, without the risk of closing it from the inside.
  If 'current, close the current popup if ESC/C-g is pressed from inside of the
    popup.
  If nil, pressing ESC/C-g will never close this buffer.
  If a function, it is checked each time ESC/C-g is pressed to determine the
    fate of the popup window. This function takes one argument: the popup
    window and must return one of the other possible values.

(select . CDR)
  CDR can be a boolean or function. The boolean determines whether to focus the
  popup window after it opens (non-nil) or focus the origin window (nil).

  If a function, it takes two arguments: the popup window and the source window
  (where you were before the popup was opened). It does nothing else, and
  ignores its return value.

(modeline . CDR)
  CDR can be t (show the default modeline), a symbol representing the name of a
  modeline defined with `def-modeline!', nil (show no modeline) or a function
  that returns one of these. The function takes one argument: the popup buffer.

(autosave . CDR)
  This parameter determines what to do with modified buffers in closing popup
  windows. CDR can be a t, 'ignore, a function or nil.

  If t, no prompts. Just save them automatically (if they're file-visiting
    buffers).
  If 'ignore, no prompts, no saving. Just silently kill it.
  If nil (the default), prompt the user what to do if the buffer is
    file-visiting and modified.
  If a function, the return value must return one of the other values. It takes
    two arguments: the popup window and buffer.

(popup . t)
  This is for internal use, do not change this. It simply marks a window as a
  popup window.

Since I can't find this information anywhere but the Emacs manual, I'll include
a brief description of some native window parameters that Emacs uses:

(delete-window . CDR)
(delete-other-window . CDR)
(split-window . CDR)
(other-window . CDR)
  This applies to all four of the above: CDR can be t or a function. If t, using
  those functions on this window will ignore all window parameters.

  If CDR is a function, it will replace the native function when used on this
  window. e.g. if CDR is #'ignore (delete-window popup) will run (ignore popup)
  instead of deleting the window!
(no-other-window . BOOL)
  If CDR is non-nil, this window becomes invisible to `other-window' and
  `pop-to-buffer'. Doom popups sets this. The default is nil.")

(defvar +popup-display-buffer-actions
  '(display-buffer-reuse-window +popup-display-buffer)
  "The functions to use to display the popup buffer.")

(defvar +popup-default-alist
  '((window-height . 0.16)
    (reusable-frames . visible))
  "The default alist for `display-buffer-alist' rules.")

(defvar +popup-default-parameters
  '((transient . t)
    (quit . t)
    (select . ignore))
  "The default window parameters.")

(defvar +popup-ttl 5
  "The default time-to-live for transient buffers whose popup buffers have been
deleted.")


;;
;; Default popup rules & bootstrap
;;

(when (featurep! +all)
  (set! :popups
    '("^ \\*" ((slot . 1) (vslot . -1) (size . +popup-shrink-to-fit)))
    '("^\\*"  ((slot . 1) (vslot . -1)) ((select . t)))))

(when (featurep! +defaults)
  (set! :popups
    '("^\\*Completions"
      ((slot . -1) (vslot . -2))
      ((transient . 0)))
    '("^\\*Compil\\(?:ation\\|e-Log\\)"
      ((size . 0.3))
      ((transient . 0) (quit . t)))
    '("^\\*\\(?:scratch\\|Messages\\)"
      nil
      ((autosave . t) (transient)))
    '("^\\*doom \\(?:term\\|eshell\\)"
      ((size . 0.25) (vslot . -10))
      ((select . t) (quit) (transient . 0)))
    '("^\\*doom:"
      ((size . 0.35) (side . bottom))
      ((autosave . t) (select . t) (modeline . t) (quit) (transient . t)))
    '("^\\*\\(?:\\(?:Pp E\\|doom e\\)val\\)"
      ((size . +popup-shrink-to-fit))
      ((transient . 0) (select . ignore)))
    '("^\\*Customize"
      ((slot . 2) (side . right))
      ((modeline . nil) (select . t) (quit . t)))
    '("^ \\*undo-tree\\*"
      ((slot . 2) (side . left) (size . 20))
      ((modeline . nil) (select . t) (quit . t)))
    ;; `help-mode', `helpful-mode'
    '("^\\*[Hh]elp"
      ((slot . 2) (vslot . 2) (size . 0.25))
      ((select . t)))
    ;; `Info-mode'
    '("^\\*info\\*$"
      ((slot . 2) (vslot . 2) (size . 0.45))
      ((select . t)))))

(add-hook 'doom-init-ui-hook #'+popup-mode)
(add-hook! '+popup-buffer-mode-hook
  #'(+popup|adjust-fringes
     +popup|set-modeline-on-enable
     +popup|unset-modeline-on-disable))


;;
;; Hacks
;;

(load! "+hacks")
