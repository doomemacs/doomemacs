;;; feature/popup/config.el -*- lexical-binding: t; -*-

(defconst +popup-window-parameters
  '(transient quit select modeline popup)
  "A list of custom parameters to be added to `window-persistent-parameters'.
Modifying this has no effect, unless done before feature/popup loads.

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

(defvar +popup-mode-map (make-sparse-keymap)
  "Active keymap in a session with the popup system enabled. See
`+popup-mode'.")

(defvar +popup-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (when (featurep! :feature evil)
      ;; for maximum escape coverage in emacs state buffers
      (define-key map [escape] #'doom/escape)
      (define-key map (kbd "ESC") #'doom/escape))
    map)
  "Active keymap in popup windows. See `+popup-buffer-mode'.")


(defvar +popup--inhibit-transient nil)
(defvar +popup--display-buffer-alist nil)
(defvar +popup--old-display-buffer-alist nil)
(defvar +popup--remember-last t)
(defvar +popup--last nil)
(defvar-local +popup--timer nil)


;;
(defun +popup-define (condition &optional alist parameters)
  "Define a popup rule.

The buffers of new windows displayed by `pop-to-buffer' and `display-buffer'
will be tested against CONDITION, which is either a) a regexp string (which is
matched against the buffer's name) or b) a function that takes no arguments and
returns a boolean.

If CONDITION is met, the buffer will be displayed in a popup window with ALIST
and window PARAMETERS. See `display-buffer-alist' for details on what ALIST may
contain and `+popup-window-parameters' for what window parameters that the popup
module supports.

ALIST also supports the `size' parameter, which will be translated to
`window-width' or `window-height' depending on `side'.

If certain attributes/parameters are omitted, the ones from
`+popup-default-alist' and `+popup-default-parameters' will be used."
  (declare (indent 1))
  (push (if (eq alist :ignore)
            (list condition nil)
          `(,condition
            (+popup-buffer)
            ,@alist
            (window-parameters ,@parameters)))
        +popup--display-buffer-alist))

(def-setting! :popup (condition &optional alist parameters)
  "Register a popup rule.

CONDITION can be a regexp string or a function. See `display-buffer' for a list
of possible entries for ALIST, which tells the display system how to initialize
the popup window. PARAMETERS is an alist of window parameters. See
`+popup-window-parameters' for a list of custom parameters provided by the popup
module.

ALIST supports one custom parameter: `size', which will resolve to
`window-height' or `window-width' depending on `side'."
  `(progn
     (+popup-define ,condition ,alist ,parameters)
     (when (bound-and-true-p +popup-mode)
       (setq display-buffer-alist +popup--display-buffer-alist))
     +popup--display-buffer-alist))

(def-setting! :popups (&rest rules)
  "Register multiple popup rules with :popup setting (`doom--set:popup'). For
example:

 (set! :popups
   (\"^ \\*\" '((slot . 1) (vslot . -1) (size . +popup-shrink-to-fit)))
   (\"^\\*\"  '((slot . 1) (vslot . -1)) '((select . t))))"
  `(progn
     ,@(cl-loop for rule in rules collect `(+popup-define ,@rule))
     (when (bound-and-true-p +popup-mode)
       (setq display-buffer-alist +popup--display-buffer-alist))
     +popup--display-buffer-alist))


;;
;; Default popup rules & bootstrap
;;

(when (featurep! +all)
  (+popup-define "^ \\*" '((slot . 1) (vslot . -1) (size . +popup-shrink-to-fit)))
  (+popup-define "^\\*"  '((slot . 1) (vslot . -1)) '((select . t))))

(when (featurep! +defaults)
  (+popup-define "^\\*Completions"
    '((slot . -1) (vslot . -2))
    '((transient . 0)))
  (+popup-define "^\\*Compil\\(?:ation\\|e-Log\\)"
    '((size . 0.3))
    '((transient . 0) (quit . t)))
  (+popup-define "^\\*\\(?:scratch\\|Messages\\)"
    nil
    '((transient)))
  (+popup-define "^\\*doom \\(?:term\\|eshell\\)"
    '((size . 0.25) (vslot . -10))
    '((select . t) (quit) (transient . 0)))
  (+popup-define "^\\*doom:"
    '((size . 0.35) (side . bottom))
    '((select . t) (modeline . t) (quit) (transient . t)))
  (+popup-define "^\\*\\(?:\\(?:Pp E\\|doom e\\)val\\)"
    '((size . +popup-shrink-to-fit))
    '((transient . 0) (select . ignore)))

  ;; `help-mode', `helpful-mode'
  (+popup-define "^\\*[Hh]elp"
    '((slot . 2) (vslot . 2) (size . 0.2))
    '((select . t)))
  ;; `Info-mode'
  (+popup-define "^\\*info\\*$"
    '((slot . 2) (vslot . 2) (size . 0.35))
    '((select . t)))

  ;; `org-mode'
  ;; Use org-load-hook instead of `after!' because the hook runs sooner,
  ;; allowing users to override these later.
  (add-hook! 'org-load-hook
    (+popup-define "^\\*\\(?:Agenda Com\\|Calendar\\|Org \\(?:Links\\|Export Dispatcher\\|Select\\)\\)"
      '((slot . -1) (vslot . -1) (size . +popup-shrink-to-fit))
      '((transient . 0)))
    (+popup-define "^\\*Org Agenda"
      '((size . 20))
      '((select . t) (transient)))
    (+popup-define "^\\*Org Src"
      '((size . 0.3))
      '((quit) (select . t)))
    (+popup-define "^CAPTURE.*\\.org$"
      '((size . 0.2))
      '((quit) (select . t)))))

(add-hook 'doom-init-ui-hook #'+popup-mode)
(add-hook! '+popup-buffer-mode-hook
  #'(+popup|adjust-fringes
     +popup|set-modeline-on-enable
     +popup|unset-modeline-on-disable))


;;
;; Hacks
;;

(load! +hacks)
