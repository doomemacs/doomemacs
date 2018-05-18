;;; ui/popup/init.el -*- lexical-binding: t; -*-

(defvar +popup--display-buffer-alist nil)

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
