;;; config.el -*- lexical-binding: t; -*-

(defconst +popup-window-parameters
  '(transient quit select modeline alist)
  "A list of custom parameters to be added to `window-persistent-parameters'.
Modifying this has no effect, unless done before feature/popup loads.

(transient . CDR)
  CDR can be t, an integer or nil. It represents the number of seconds before
  the buffer belonging to a closed popup window is killed.

  If t, CDR will default to `+popup-ttl'.
  If 0, the buffer is immediately killed.
  If nil, the buffer won't be killed.

(quit . CDR)
  CDR can be t, 'other, 'current or nil. This determines the behavior of the
  ESC/C-g keys in or outside of popup windows.

  If t, close the popup if ESC/C-g is pressed inside or outside of popups.
  If 'other, close this popup if ESC/C-g is pressed outside of any popup. This
    is great for popups you just want to peek at and discard.
  If 'current, close the current popup if ESC/C-g is pressed from inside of the
    popup.
  If nil, pressing ESC/C-g will never close this buffer.

(select . BOOl)
  CDR is a boolean that determines whether to focus the popup window after it
  opens.

(modeline . CDR)
  CDR can be t (show the default modeline), a symbol representing the name of a
  modeline defined with `def-modeline!', or nil (show no modeline).

(alist . CDR)
  This is an internal parameter and should not be set or modified.

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

(defvar +popup-default-alist
  '((slot . 1)
    (window-height . 0.14)
    (window-width . 26)
    (reusable-frames . visible))
  "The default alist for `display-buffer-alist' rules.")

(defvar +popup-default-parameters
  '((transient . t)
    (quit . t))
  "The default window parameters to add alists fed to `display-buffer-alist'.")

(defvar +popup-ttl 10
  "The default time-to-live for transient buffers whose popup buffers have been
deleted.")

(defvar +popup-mode-map (make-sparse-keymap)
  "Active keymap in a session with the popup system enabled. See
`+popup-mode'.")

(defvar +popup-buffer-mode-map (make-sparse-keymap)
  "Active keymap in popup windows. See `+popup-buffer-mode'.")


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
     ,(when alist
        '(when-let* ((size (cdr (assq 'size alist))))
           (setq alist (assq-delete-all 'size alist))
           (push (cons (pcase (cdr (or (assq 'side alist)
                                       (assq 'side +popup-default-alist)))
                         ((or `left `right) 'window-width)
                         (_ 'window-height))
                       size)
                 alist)))
     (prog1 (push (append (list ,condition '(+popup-buffer))
                          alist
                          (list (cons 'window-parameters parameters)))
                  +popup--display-buffer-alist)
       (when (bound-and-true-p +popup-mode)
         (setq display-buffer-alist +popup--display-buffer-alist)))))


;;
;; Default popup rules & bootstrap
;;

(eval-when-compile
  (set! :popup "^ \\*")
  (set! :popup "^\\*" nil '((select . t)))
  (set! :popup "^\\*\\(?:scratch\\|Messages\\)" nil '((transient)))
  (set! :popup "^\\*Help"
    '((size . 0.2))
    '((select . t)))
  (set! :+popup "^\\*doom:"
    '((size . 0.35))
    '((select . t) (quit) (transient))))

(setq +popup--display-buffer-alist (eval-when-compile +popup--display-buffer-alist))
(add-hook 'doom-init-ui-hook #'+popup-mode)

(add-hook '+popup-buffer-mode-hook #'+popup|adjust-fringes)
(add-hook '+popup-buffer-mode-hook #'+popup|set-modeline)


;;
;; Hacks
;;

(defun doom*ignore-window-parameters (orig-fn &rest args)
  "Allow *interactive* window moving commands to traverse popups."
  (cl-letf (((symbol-function #'windmove-find-other-window)
             (lambda (direction &optional window ignore sign wrap mini)
               (window-in-direction
                (pcase dir (`up 'above) (`down 'below) (_ dir))
                window (bound-and-true-p +popup-mode) arg windmove-wrap-around t))))
    (apply orig-fn args)))
(advice-add #'windmove-up :around #'doom*ignore-window-parameters)
(advice-add #'windmove-down :around #'doom*ignore-window-parameters)
(advice-add #'windmove-left :around #'doom*ignore-window-parameters)
(advice-add #'windmove-right :around #'doom*ignore-window-parameters)


(after! help-mode
  (defun doom--switch-from-popup (location)
    (let (origin)
      (save-popups!
       (switch-to-buffer (car location) nil t)
       (if (not (cdr location))
           (message "Unable to find location in file")
         (goto-char (cdr location))
         (recenter)
         (setq origin (selected-window))))
      (+popup/close)
      (select-window origin)))

  ;; Help buffers use `pop-to-window' to decide where to open followed links,
  ;; which can be unpredictable. It should *only* replace the original buffer we
  ;; opened the popup from. To fix this these three button types need to be
  ;; redefined to set aside the popup before following a link.
  (define-button-type 'help-function-def
    :supertype 'help-xref
    'help-function
    (lambda (fun file)
      (require 'find-func)
      (when (eq file 'C-source)
        (setq file (help-C-file-name (indirect-function fun) 'fun)))
      (doom--switch-from-popup (find-function-search-for-symbol fun nil file))))

  (define-button-type 'help-variable-def
    :supertype 'help-xref
    'help-function
    (lambda (var &optional file)
      (when (eq file 'C-source)
        (setq file (help-C-file-name var 'var)))
      (doom--switch-from-popup (find-variable-noselect var file))))

  (define-button-type 'help-face-def
    :supertype 'help-xref
    'help-function
    (lambda (fun file)
      (require 'find-func)
      (doom--switch-from-popup (find-function-search-for-symbol fun 'defface file)))))

(provide 'config)
;;; config.el ends here
