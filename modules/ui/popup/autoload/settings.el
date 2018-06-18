;;; ui/popup/autoload/settings.el -*- lexical-binding: t; -*-

(defvar +popup--display-buffer-alist nil)

;;;###autoload
(defvar +popup-defaults
  (list :side   'bottom
        :height 0.16
        :width  40
        :quit   t
        :select #'ignore
        :ttl    5)
  "Default setup for `set-popup-rule!' ")

;;;###autoload
(defun +popup--make (predicate plist)
  (cond ((not (keywordp (car plist)))
         ;; FIXME deprecated popup rule support
         (message "Warning: the old usage of `set-popup-rule!' is deprecated; update the rule for '%s'"
                  predicate)
         (cl-destructuring-bind (condition &optional alist parameters)
             (list predicate (car plist) (cadr plist))
           (if (eq alist :ignore)
               (list condition nil)
             `(,condition (+popup-buffer)
                          ,@alist
                          (window-parameters ,@parameters)))))
        ((plist-get plist :ignore)
         (list predicate nil))
        ((let* ((plist (append plist +popup-defaults))
                (alist
                 `((actions       . ,(plist-get plist :actions))
                   (side          . ,(plist-get plist :side))
                   (size          . ,(plist-get plist :size))
                   (window-width  . ,(plist-get plist :width))
                   (window-height . ,(plist-get plist :height))
                   (slot          . ,(plist-get plist :slot))
                   (vslot         . ,(plist-get plist :vslot))))
                (params
                 `((ttl      . ,(plist-get plist :ttl))
                   (quit     . ,(plist-get plist :quit))
                   (select   . ,(plist-get plist :select))
                   (modeline . ,(plist-get plist :modeline))
                   (autosave . ,(plist-get plist :autosave))
                   ,@(plist-get plist :parameters))))
           `(,predicate (+popup-buffer)
                        ,@alist
                        (window-parameters ,@params))))))

;;;###autodef
(defun set-popup-rule! (predicate &rest plist)
  "Define a popup rule.

Buffers displayed by `pop-to-buffer' and `display-buffer' (or their siblings)
will be tested against PREDICATE, which is either a) a regexp string (which is
matched against the buffer's name) or b) a function that takes no arguments and
returns a boolean.

Buffers displayed with `switch-to-buffer' and its variants will not be affected
by these rules (as they are unaffected by `display-buffer-alist', which powers
the popup management system).

PLIST can be made up of any of the following properties:

:actions ACTIONS
  ACTIONS is a list of functions or an alist containing (FUNCTION . ALIST). See
  `display-buffer''s second argument for more information on its format and what
  it accepts. If omitted, `+popup-default-display-buffer-actions' is used.

:side 'bottom|'top|'left|'right
  Which side of the frame to open the popup on. This is only respected if
  `+popup-display-buffer-stacked-side-window' or `display-buffer-in-side-window'
  is in :actions or `+popup-default-display-buffer-actions'.

:size/:width/:height FLOAT|INT
  Determines the size of the popup. If opened at the top or bottom, the width is
  irrelevant unless it is opened in an adjacent slot. Same deal with the left
  and right side.

  If given a FLOAT (0 < x < 1), the number represents how much of the window
    will be consumed by the popup (a percentage).
  If given an INT, the number determines the size in lines (height) or units of
    character width (width).

:slot/:vslot INT
  This only applies to popups with a :side. For popups opened at the top or
  bottom, slot designates the horizontal positioning of a popup. If two popups
  are assigned the same slot (and same vslot), the later popup will replace the
  earlier one. If the later popup has a lower slot, it will open to the older
  popup's left. A higher slot opens it to the old popup's right.

  On the other hand, vslot operates the same way, but controls how popups are
  stacked.

  When a popup is opened on the left and right, slot determines vertical
  position and vslot horizontal.

:ttl INT|BOOL|FN
  Stands for time-to-live. CDR can be t, an integer, nil or a function that
  returns one of these. It represents the number of seconds before the buffer
  belonging to a closed popup window is killed.

  If t, CDR will default to `+popup-ttl'.
  If 0, the buffer is immediately killed.
  If nil, the buffer won't be killed.
  If a function, it must return one of the other possible values above. It takes
    the popup buffer as its sole argument.

:quit BOOL|FN
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
    fate of the popup window. This function takes one argument: the popup window
    and must return one of the other possible values.

:select BOOL|FN
  CDR can be a boolean or function. The boolean determines whether to focus the
  popup window after it opens (non-nil) or focus the origin window (nil).

  If a function, it takes two arguments: the popup window and the source window
  (where you were before the popup was opened). It does nothing else, and
  ignores its return value.

:modeline BOOL|SYMBOL|FN
  CDR can be t (show the default modeline), a symbol representing the name of a
  modeline defined with `def-modeline!', nil (show no modeline) or a function
  that returns one of these. The function takes one argument: the popup buffer.

:autosave BOOL|FN
  This parameter determines what to do with modified buffers in closing popup
  windows. CDR can be a t, 'ignore, a function or nil.

  If t, no prompts. Just save them automatically (if they're file-visiting
    buffers).
  If 'ignore, no prompts, no saving. Just silently kill it.
  If nil (the default), prompt the user what to do if the buffer is
    file-visiting and modified.
  If a function, the return value must return one of the other values. It takes
    two arguments: the popup window and buffer.

:parameters ALIST
  An alist of custom window parameters. See \(info window-parameters)

If any of these are omitted, defaults derived from `+popup-defaults' will be
used."
  (declare (indent defun))
  (push (+popup--make predicate plist) +popup--display-buffer-alist)
  (when (bound-and-true-p +popup-mode)
    (setq display-buffer-alist +popup--display-buffer-alist))
  +popup--display-buffer-alist)

;;;###autodef
(defun set-popup-rules! (&rest rulesets)
  "Like `set-popup-rules!', but defines multiple popup rules. Every entry in RULESETS
should be a list of lists (each sublist is a popup rule that could be passed to
`set-popup-rule!').

Example:

  (set-popup-rules!
    '((\"^ \\*\" :slot 1 :vslot -1 :size #'+popup-shrink-to-fit)
      (\"^\\*\"  :slot 1 :vslot -1 :select t))
    '((\"^\\*Completions\" :slot -1 :vslot -2 :ttl 0)
      (\"^\\*Compil\\(?:ation\\|e-Log\\)\" :size 0.3 :ttl 0 :quit t)))"
  (declare (indent 0))
  (dolist (rules rulesets)
    (dolist (rule rules)
      (push (+popup--make (car rule) (cdr rule))
            +popup--display-buffer-alist)))
  (when (bound-and-true-p +popup-mode)
    (setq display-buffer-alist +popup--display-buffer-alist))
  +popup--display-buffer-alist)


;;
;; Obsolete settings
;;

;; FIXME obsolete :popup
;;;###autoload
(def-setting! :popup (condition &optional alist parameters)
  :obsolete set-popup-rule!
  `(set-popup-rule! ,condition ,alist ,parameters))

;; FIXME obsolete :popups
;;;###autoload
(def-setting! :popups (&rest rulesets)
  :obsolete set-popup-rules!
  `(set-popup-rules! ,@rulesets))
