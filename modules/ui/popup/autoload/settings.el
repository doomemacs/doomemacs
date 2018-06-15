;;; ui/popup/autoload/settings.el -*- lexical-binding: t; -*-

(defvar +popup--display-buffer-alist nil)

(defsubst +popup--rule (args)
  (cl-destructuring-bind (condition &optional alist parameters) args
    (if (eq alist :ignore)
        (list condition nil)
      `(,condition (+popup-buffer)
                   ,@alist
                   (window-parameters ,@parameters)))))

(defun +popup--define (condition &optional alist parameters)
  (when after-init-time
    (setq +popup--display-buffer-alist
          (map-delete +popup--display-buffer-alist condition)))
  (push (+popup--rule (list condition alist parameters))
        +popup--display-buffer-alist))

;;;###autodef
(defun set-popup-rule! (condition &optional alist parameters)
  "Define a popup rule.

CONDITION can be a regexp string or a function.

For ALIST, see `display-buffer' and `display-buffer-alist' for a list of
possible entries, which instruct the display system how to initialize the popup
window.

ALIST also supports the `size' parameter, which will be translated to
`window-width' or `window-height' depending on `side'.

PARAMETERS is an alist of window parameters. See `+popup-window-parameters' for
a list of custom parameters provided by the popup module. If certain
attributes/parameters are omitted, the ones from `+popup-default-alist' and
`+popup-default-parameters' will be used.

The buffers of new windows displayed by `pop-to-buffer' and `display-buffer'
will be tested against CONDITION, which is either a) a regexp string (which is
matched against the buffer's name) or b) a function that takes no arguments and
returns a boolean.

See `def-popups!' for defining multiple rules in bulk."
  (+popup--define condition alist parameters)
  (when (bound-and-true-p +popup-mode)
    (setq display-buffer-alist +popup--display-buffer-alist))
  +popup--display-buffer-alist)

;;;###autodef
(defun set-popup-rules! (&rest rulesets)
  "Define multiple popup rules. See `def-popup!' for the specifications of each
individual rule.

 (set-popup-rules!
   '((\"^ \\*\" ((slot . 1) (vslot . -1) (size . +popup-shrink-to-fit)))
     (\"^\\*\"  ((slot . 1) (vslot . -1)) ((select . t)))))"
  (dolist (ruleset rulesets)
    (dolist (rule ruleset)
      (apply #'+popup--define rule)))
  (when (bound-and-true-p +popup-mode)
    (setq display-buffer-alist +popup--display-buffer-alist))
  +popup--display-buffer-alist)


;;
;; Obsolete
;;

;; FIXME obsolete :popup
;;;###autoload
(def-setting! :popup (condition &optional alist parameters)
  "Define a popup rule.

CONDITION can be a regexp string or a function.

For ALIST, see `display-buffer' and `display-buffer-alist' for a list of
possible entries, which instruct the display system how to initialize the popup
window.

ALIST also supports the `size' parameter, which will be translated to
`window-width' or `window-height' depending on `side'.

PARAMETERS is an alist of window parameters. See `+popup-window-parameters' for
a list of custom parameters provided by the popup module. If certain
attributes/parameters are omitted, the ones from `+popup-default-alist' and
`+popup-default-parameters' will be used.

The buffers of new windows displayed by `pop-to-buffer' and `display-buffer'
will be tested against CONDITION, which is either a) a regexp string (which is
matched against the buffer's name) or b) a function that takes no arguments and
returns a boolean.

See `def-popups!' for defining multiple rules in bulk."
  :obsolete set-popup-rule!
  `(set-popup-rule! ,condition ,alist ,parameters))

;; FIXME obsolete :popups
;;;###autoload
(def-setting! :popups (&rest rulesets)
  "Define multiple popup rules. See `def-popup!' for the specifications of each
individual rule.

 (set-popup-rules!
   '((\"^ \\*\" ((slot . 1) (vslot . -1) (size . +popup-shrink-to-fit)))
     (\"^\\*\"  ((slot . 1) (vslot . -1)) ((select . t)))))"
  :obsolete set-popup-rules!
  `(set-popup-rules! ,@rulesets))
