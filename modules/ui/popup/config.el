;;; ui/popup/config.el -*- lexical-binding: t; -*-

(defconst +popup-window-parameters '(ttl quit select modeline popup)
  "A list of custom parameters to be added to `window-persistent-parameters'.
Modifying this has no effect, unless done before ui/popup loads.")

(defvar +popup-default-display-buffer-actions
  '(display-buffer-reuse-window +popup-display-buffer-stacked-side-window)
  "The functions to use to display the popup buffer.")

(defvar +popup-default-alist
  '((window-height . 0.16) ; remove later
    (reusable-frames . visible))
  "The default alist for `display-buffer-alist' rules.")

(defvar +popup-default-parameters
  '((transient . t)   ; remove later
    (quit . t)        ; remove later
    (select . ignore) ; remove later
    (no-other-window . t))
  "The default window parameters.")

(defvar +popup-margin-width 1
  "Size of the margins to give popup windows. Set this to nil to disable margin
adjustment.")

(defvar +popup--populate-wparams (not EMACS26+))
(defvar +popup--inhibit-transient nil)
(defvar +popup--inhibit-select nil)
(defvar +popup--old-display-buffer-alist nil)
(defvar +popup--remember-last t)
(defvar +popup--last nil)
(defvar-local +popup--timer nil)


;;
;; Global modes

(defvar +popup-mode-map (make-sparse-keymap)
  "Active keymap in a session with the popup system enabled. See
`+popup-mode'.")

(defvar +popup-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (when (featurep! :feature evil)
      ;; For maximum escape coverage in emacs state buffers; this only works in
      ;; GUI Emacs, in tty Emacs use C-g instead
      (define-key map [escape] #'doom/escape))
    map)
  "Active keymap in popup windows. See `+popup-buffer-mode'.")

(define-minor-mode +popup-mode
  "Global minor mode representing Doom's popup management system."
  :init-value nil
  :global t
  :keymap +popup-mode-map
  (cond (+popup-mode
         (add-hook 'doom-escape-hook #'+popup|close-on-escape t)
         (add-hook 'doom-cleanup-hook #'+popup|cleanup-rules)
         (setq +popup--old-display-buffer-alist display-buffer-alist
               display-buffer-alist +popup--display-buffer-alist
               window--sides-inhibit-check t)
         (dolist (prop +popup-window-parameters)
           (push (cons prop 'writable) window-persistent-parameters)))
        (t
         (remove-hook 'doom-escape-hook #'+popup|close-on-escape)
         (remove-hook 'doom-cleanup-hook #'+popup|cleanup-rules)
         (setq display-buffer-alist +popup--old-display-buffer-alist
               window--sides-inhibit-check nil)
         (+popup|cleanup-rules)
         (dolist (prop +popup-window-parameters)
           (delq (assq prop window-persistent-parameters)
                 window-persistent-parameters)))))

(define-minor-mode +popup-buffer-mode
  "Minor mode for individual popup windows.

It is enabled when a buffer is displayed in a popup window and disabled when
that window has been changed or closed."
  :init-value nil
  :keymap +popup-buffer-mode-map
  (if (not +popup-buffer-mode)
      (remove-hook 'after-change-major-mode-hook #'+popup|set-modeline-on-enable t)
    (add-hook 'after-change-major-mode-hook #'+popup|set-modeline-on-enable nil t)
    (when (timerp +popup--timer)
      (remove-hook 'kill-buffer-hook #'+popup|kill-buffer-hook t)
      (cancel-timer +popup--timer)
      (setq +popup--timer nil))))

(put '+popup-buffer-mode 'permanent-local t)
(put '+popup-buffer-mode 'permanent-local-hook t)
(put '+popup|set-modeline-on-enable 'permanent-local-hook t)


;;
;; Macros

(defmacro with-popup-rules! (rules &rest body)
  "Evaluate BODY with popup RULES. RULES is a list of popup rules. Each rule
should match the arguments of `+popup-define' or the :popup setting."
  (declare (indent defun))
  `(let ((+popup--display-buffer-alist +popup--old-display-buffer-alist)
         display-buffer-alist)
     (set-popup-rules! ,rules)
     (when (bound-and-true-p +popup-mode)
       (setq display-buffer-alist +popup--display-buffer-alist))
     ,@body))

(defmacro save-popups! (&rest body)
  "Sets aside all popups before executing the original function, usually to
prevent the popup(s) from messing up the UI (or vice versa)."
  `(let* ((in-popup-p (+popup-buffer-p))
          (popups (+popup-windows))
          (+popup--inhibit-transient t)
          +popup--last)
     (dolist (p popups)
       (+popup/close p 'force))
     (unwind-protect
         (progn ,@body)
       (when popups
         (let ((origin (selected-window)))
           (+popup/restore)
           (unless in-popup-p
             (select-window origin)))))))


;;
;; Default popup rules & bootstrap

(set-popup-rules!
  (when (featurep! +all)
    '(("^\\*"  :slot 1 :vslot -1 :select t)
      ("^ \\*" :slot 1 :vslot -1 :size +popup-shrink-to-fit)))
  (when (featurep! +defaults)
    '(("^\\*bin/doom\\*$"
       :vslot 9999 :size 0.75 :quit 'current :select t :ttl 0)
      ("^\\*Completions"
       :slot -1 :vslot -2 :ttl 0)
      ("^\\*Compil\\(?:ation\\|e-Log\\)"
       :vslot -2 :size 0.3 :ttl 0 :quit t)
      ("^\\*\\(?:scratch\\|Messages\\)"
       :autosave t :ttl nil)
      ("^\\*Man "
       :size 0.45 :vslot -3 :ttl 0 :quit t :select t)
      ("^\\*doom \\(?:term\\|eshell\\)"
       :size 0.25 :vslot -4 :select t :quit nil :ttl 0)
      ("^\\*doom:"
       :vslot -5 :size 0.35 :size bottom :autosave t :select t :modeline t :quit nil)
      ("^\\*\\(?:\\(?:Pp E\\|doom e\\)val\\)"
       :size +popup-shrink-to-fit :ttl 0 :select ignore)
      ("^\\*Customize"
       :slot 2 :side right :select t :quit t)
      ("^ \\*undo-tree\\*"
       :slot 2 :side left :size 20 :select t :quit t)
      ;; `help-mode', `helpful-mode'
      ("^\\*[Hh]elp"
       :slot 2 :vslot -2 :size 0.35 :select t)
      ;; `eww' (and used by dash docsets)
      ("^\\*eww\\*"
       :vslot -11 :size 0.35 :select t)
      ;; `Info-mode'
      ("^\\*info\\*$"
       :slot 2 :vslot 2 :size 0.45 :select t)))
  '(("^\\*Backtrace" :vslot 99 :size 0.4 :quit nil)))

(add-hook 'doom-init-ui-hook #'+popup-mode :append)

(add-hook! '+popup-buffer-mode-hook
  #'(+popup|adjust-fringes
     +popup|adjust-margins
     +popup|set-modeline-on-enable
     +popup|unset-modeline-on-disable))


;;
;; Hacks

(load! "+hacks")
