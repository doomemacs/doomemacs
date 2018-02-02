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

(defvar +popup-ttl 10
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
(def-setting! :popup (condition &optional alist parameters)
  "Register a popup rule.

CONDITION can be a regexp string or a function. See `display-buffer' for a list
of possible entries for ALIST, which tells the display system how to initialize
the popup window. PARAMETERS is an alist of window parameters. See
`+popup-window-parameters' for a list of custom parameters provided by the popup
module.

ALIST supports one custom parameter: `size', which will resolve to
`window-height' or `window-width' depending on `side'."
  `(let ((alist ,alist)
         (parameters ,parameters))
     (if (eq alist :ignore)
         (push (list ,condition nil) +popup--display-buffer-alist)
       ,(when alist
          `(when-let* ((size (cdr (assq 'size alist)))
                       (side (or (cdr (assq 'side (append alist +popup-default-alist)))
                                 'bottom)))
             (map-delete alist 'size)
             (map-put alist (if (memq side '(left right))
                                'window-width
                              'window-height)
                      size)))
       (prog1 (push (append (list ,condition '(+popup-buffer))
                            alist
                            (list (cons 'window-parameters parameters)))
                    +popup--display-buffer-alist)))
     (when (bound-and-true-p +popup-mode)
       (setq display-buffer-alist +popup--display-buffer-alist))
     nil))


;;
;; Default popup rules & bootstrap
;;

(eval-when-compile
  (when (featurep! +all)
    (set! :popup "^ \\*" '((slot . 1) (vslot . -1) (size . +popup-shrink-to-fit)))
    (set! :popup "^\\*"  '((slot . 1) (vslot . -1)) '((select . t))))

  (when (featurep! +defaults)
    (set! :popup "^\\*Completions" '((slot . -1) (vslot . -2)) '((transient . 0)))
    (set! :popup "^\\*Compil\\(ation\\|e-Log\\)" nil '((transient . 0) (quit . t)))
    (set! :popup "^\\*\\(?:scratch\\|Messages\\)" nil '((transient)))
    (set! :popup "^\\*[Hh]elp"
      '((slot . 2) (vslot . 2) (size . 0.2))
      '((select . t)))
    (set! :popup "^\\*doom \\(?:term\\|eshell\\)"
      '((size . 0.25))
      '((quit) (transient . 0)))
    (set! :popup "^\\*doom:"
      '((size . 0.35) (side . bottom))
      '((select . t) (modeline . t) (quit) (transient . t)))
    (set! :popup "^\\*\\(?:\\(?:Pp E\\|doom e\\)val\\)"
      '((size . +popup-shrink-to-fit)) '((transient . 0) (select . ignore))))
  nil)

(setq +popup--display-buffer-alist (eval-when-compile +popup--display-buffer-alist))

(add-hook 'doom-init-ui-hook #'+popup-mode)
(add-hook! '+popup-buffer-mode-hook #'(+popup|adjust-fringes +popup|set-modeline))


;;
;; Hacks
;;

(load! +hacks)
