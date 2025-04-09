;;; ui/popup/autoload/popup.el -*- lexical-binding: t; -*-

(defvar +popup--internal nil)

(defun +popup--remember (windows)
  "Remember WINDOWS (a list of windows) for later restoration."
  (cl-assert (cl-every #'windowp windows) t)
  (setq +popup--last
        (cl-loop for w in windows
                 collect (cons (window-buffer w)
                               (window-state-get w)))))

(defun +popup--kill-buffer (buffer ttl)
  "Tries to kill BUFFER, as was requested by a transient timer. If it fails, eg.
the buffer is visible, then set another timer and try again later."
  (let ((inhibit-quit t))
    (cond ((not (buffer-live-p buffer)))
          ((not (get-buffer-window buffer t))
           (with-demoted-errors "Error killing transient buffer: %s"
             (with-current-buffer buffer
               (let ((kill-buffer-hook (remq '+popup-kill-buffer-hook-h kill-buffer-hook))
                     confirm-kill-processes)
                 (when-let (process (get-buffer-process buffer))
                   (when (eq (process-type process) 'real)
                     (kill-process process)))
                 (let (kill-buffer-query-functions)
                   ;; HACK The debugger backtrace buffer, when killed, called
                   ;;      `top-level'. This causes jumpiness when the popup
                   ;;      manager tries to clean it up.
                   (cl-letf (((symbol-function #'top-level) #'ignore))
                     (kill-buffer buffer)))))))
          ((let ((ttl (if (= ttl 0)
                          (or (plist-get +popup-defaults :ttl) 3)
                        ttl)))
             (with-current-buffer buffer
               (setq +popup--timer
                     (run-at-time ttl nil #'+popup--kill-buffer buffer ttl))))))))

(defun +popup--delete-window (window)
  "Do housekeeping before destroying a popup window.

+ Disables `+popup-buffer-mode' so that any hooks attached to it get a chance to
  run and do cleanup of its own.
+ Either kills the buffer or sets a transient timer, if the window has a
  `transient' window parameter (see `+popup-window-parameters').
+ And finally deletes the window!"
  (let ((buffer (window-buffer window))
        (inhibit-quit t))
    (and (or (buffer-file-name buffer)
             (if-let* ((base-buffer (buffer-base-buffer buffer)))
                 (buffer-file-name base-buffer)))
         (buffer-modified-p buffer)
         (let ((autosave (+popup-parameter 'autosave window)))
           (cond ((eq autosave 't))
                 ((null autosave)
                  (y-or-n-p "Popup buffer is modified. Save it?"))
                 ((functionp autosave)
                  (funcall autosave buffer))))
         (with-current-buffer buffer (save-buffer)))
    (let ((ignore-window-parameters t))
      (if-let* ((wconf (window-parameter window 'saved-wconf)))
          (set-window-configuration wconf)
        (delete-window window)))
    (unless (window-live-p window)
      (with-current-buffer buffer
        (set-buffer-modified-p nil)
        (+popup-buffer-mode -1)
        (unless +popup--inhibit-transient
          (let ((ttl (+popup-parameter 'ttl window)))
            (when (eq ttl 't)
              (setq ttl (plist-get +popup-defaults :ttl)))
            (cond ((null ttl))
                  ((functionp ttl)
                   (funcall ttl buffer))
                  ((not (integerp ttl))
                   (signal 'wrong-type-argument (list 'integerp ttl)))
                  ((= ttl 0)
                   (+popup--kill-buffer buffer 0))
                  ((add-hook 'kill-buffer-hook #'+popup-kill-buffer-hook-h nil t)
                   (setq +popup--timer
                         (run-at-time ttl nil #'+popup--kill-buffer
                                      buffer ttl))))))))))

(defun +popup--delete-other-windows (window)
  "Fixes `delete-other-windows' when used from a popup window."
  (when-let (window (ignore-errors (+popup/raise window)))
    (let ((ignore-window-parameters t))
      (delete-other-windows window)))
  nil)

(defun +popup--normalize-alist (alist)
  "Merge `+popup-default-alist' and `+popup-default-parameters' with ALIST."
  (when alist
    (let ((alist  ; handle defaults
           (cl-remove-duplicates
            (append alist +popup-default-alist)
            :key #'car-safe :from-end t))
          (parameters
           (cl-remove-duplicates
            (append (cdr (assq 'window-parameters alist))
                    +popup-default-parameters)
            :key #'car-safe :from-end t)))
      ;; handle `size'
      (when-let* ((size  (cdr (assq 'size alist)))
                  (side  (or (cdr (assq 'side alist)) 'bottom))
                  (param (if (memq side '(left right))
                             'window-width
                           'window-height)))
        (setq alist (assq-delete-all 'size alist))
        (setf (alist-get param alist) size))
      (setf (alist-get 'window-parameters alist)
            parameters)
      ;; Fixes #1305: addresses an edge case where a popup with a :size, :width
      ;; or :height greater than the current frame's dimensions causes
      ;; hanging/freezing (a bug in Emacs' `display-buffer' API perhaps?)
      (let ((width  (cdr (assq 'window-width  alist)))
            (height (cdr (assq 'window-height alist))))
        (setf (alist-get 'window-width alist)
              (if (numberp width)
                  (min width (frame-width))
                width))
        (setf (alist-get 'window-height alist)
              (if (numberp height)
                  (min height (frame-height))
                height))
        alist))))

(defun +popup--split-window (window size side)
  "Ensure a non-dedicated/popup window is selected when splitting a window."
  (unless +popup--internal
    (cl-loop for win
             in (cons (or window (selected-window))
                      (window-list nil 0 window))
             unless (+popup-window-p win)
             return (setq window win)))
  (let ((ignore-window-parameters t))
    (split-window window size side)))

(defun +popup--maybe-select-window (window origin)
  "Select a window based on `+popup--inhibit-select' and this window's `select' parameter."
  (unless +popup--inhibit-select
    ;; REVIEW: Once our minimum version is bumped up to Emacs 30.x, replace this
    ;;   with `post-command-select-window' window parameter.
    (let ((select (+popup-parameter 'select window)))
      (if (functionp select)
          (funcall select window origin)
        (select-window (if select window origin))))))

;;;###autoload
(defun +popup--init (window &optional alist)
  "Initializes a popup window. Run any time a popup is opened. It sets the
default window parameters for popup windows, clears leftover transient timers
and enables `+popup-buffer-mode'."
  (with-selected-window window
    (setq alist (delq (assq 'actions alist) alist))
    (set-window-parameter window 'popup t)
    (set-window-parameter window 'split-window #'+popup--split-window)
    (set-window-parameter window 'delete-window #'+popup--delete-window)
    (set-window-parameter window 'delete-other-windows #'+popup--delete-other-windows)
    (set-window-dedicated-p window 'popup)
    (window-preserve-size
     window (memq (window-parameter window 'window-side)
                  '(left right))
     t)
    (+popup-buffer-mode +1)
    (run-hooks '+popup-create-window-hook)))


;;
;; Public library

;;;###autoload
(defun +popup-buffer-p (&optional buffer)
  "Return non-nil if BUFFER is a popup buffer. Defaults to the current buffer."
  (when +popup-mode
    (let ((buffer (or buffer (current-buffer))))
      (and (bufferp buffer)
           (buffer-live-p buffer)
           (buffer-local-value '+popup-buffer-mode buffer)
           buffer))))

;;;###autoload
(defun +popup-window-p (&optional window)
  "Return non-nil if WINDOW is a popup window. Defaults to the current window."
  (when +popup-mode
    (let ((window (or window (selected-window))))
      (and (windowp window)
           (window-live-p window)
           (window-parameter window 'popup)
           window))))

;;;###autoload
(defun +popup-buffer (buffer &optional alist)
  "Open BUFFER in a popup window. ALIST describes its features."
  (let* ((origin (selected-window))
         (window-min-height 3)
         (alist (+popup--normalize-alist alist))
         (actions (or (cdr (assq 'actions alist))
                      +popup-default-display-buffer-actions)))
    (or (let* ((alist (remove (assq 'window-width alist) alist))
               (alist (remove (assq 'window-height alist) alist))
               (window (display-buffer-reuse-window buffer alist)))
          (when window
            (+popup--maybe-select-window window origin)
            window))
        (when-let (popup (cl-loop for func in actions
                                  if (funcall func buffer alist)
                                  return it))
          (+popup--init popup alist)
          (+popup--maybe-select-window popup origin)
          popup))))

;;;###autoload
(defun +popup-parameter (parameter &optional window)
  "Fetch the window PARAMETER (symbol) of WINDOW"
  (window-parameter (or window (selected-window)) parameter))

;;;###autoload
(defun +popup-parameter-fn (parameter &optional window &rest args)
  "Fetch the window PARAMETER (symbol) of WINDOW. If it is a function, run it
with ARGS to get its return value."
  (let ((val (+popup-parameter parameter window)))
    (if (functionp val)
        (apply val args)
      val)))

;;;###autoload
(defun +popup-windows ()
  "Returns a list of all popup windows."
  (cl-remove-if-not #'+popup-window-p (window-list)))

;;;###autoload
(defun +popup-shrink-to-fit (&optional window)
  "Shrinks WINDOW to fit the buffer contents, if the buffer isn't empty.

Uses `shrink-window-if-larger-than-buffer'."
  (unless window
    (setq window (selected-window)))
  (unless (= (- (point-max) (point-min)) 0)
    (shrink-window-if-larger-than-buffer window)))

;;;###autoload
(defun +popup-alist-from-window-state (state)
  "Convert window STATE (from `window-state-get') to a `display-buffer' alist."
  (let* ((params (alist-get 'parameters state)))
    `((side          . ,(alist-get 'window-side params))
      (window-width  . ,(alist-get 'total-width state))
      (window-height . ,(alist-get 'total-height state))
      (window-parameters ,@params))))


;;
;; Hooks

;;;###autoload
(defun +popup-adjust-fringes-h ()
  "Hides the fringe in popup windows, restoring them if `+popup-buffer-mode' is
disabled."
  (when (+popup-window-p)
    (let ((f (if (bound-and-true-p +popup-buffer-mode) 0)))
      (set-window-fringes nil f f fringes-outside-margins))))

;;;###autoload
(defun +popup-adjust-margins-h ()
  "Creates padding for the popup window determined by `+popup-margin-width',
restoring it if `+popup-buffer-mode' is disabled."
  (when (and +popup-margin-width (+popup-window-p))
    (unless (memq (window-parameter nil 'window-side) '(left right))
      (let ((m (if (bound-and-true-p +popup-buffer-mode) +popup-margin-width)))
        (set-window-margins nil m m)))))

(defvar hide-mode-line-format)
;;;###autoload
(defun +popup-set-modeline-on-enable-h ()
  "Don't show modeline in popup windows without a `modeline' window-parameter.
Possible values for this parameter are:

  t            show the mode-line as normal
  nil          hide the modeline entirely (the default)
  a function   `mode-line-format' is set to its return value

Any non-nil value besides the above will be used as the raw value for
`mode-line-format'."
  (when (bound-and-true-p +popup-buffer-mode)
    (let ((modeline (+popup-parameter 'modeline)))
      (cond ((eq modeline 't))
            ((null modeline)
             ;; TODO use `mode-line-format' window parameter instead (emacs 26+)
             (hide-mode-line-mode +1))
            ((let ((hide-mode-line-format
                    (if (functionp modeline)
                        (funcall modeline)
                      modeline)))
               (hide-mode-line-mode +1)))))))
(put '+popup-set-modeline-on-enable-h 'permanent-local-hook t)

;;;###autoload
(defun +popup-unset-modeline-on-disable-h ()
  "Restore the modeline when `+popup-buffer-mode' is deactivated."
  (when (and (not (bound-and-true-p +popup-buffer-mode))
             (bound-and-true-p hide-mode-line-mode)
             (not (bound-and-true-p global-hide-mode-line-mode)))
    (hide-mode-line-mode -1)))

;;;###autoload
(defun +popup-close-on-escape-h ()
  "If called inside a popup, try to close that popup window (see
`+popup/close'). If called outside, try to close all popup windows (see
`+popup/close-all')."
  (if (+popup-window-p)
      (+popup/close)
    (+popup/close-all)))

;;;###autoload
(defun +popup-cleanup-rules-h ()
  "Cleans up any duplicate popup rules."
  (interactive)
  (setq +popup--display-buffer-alist
        (cl-delete-duplicates +popup--display-buffer-alist
                              :key #'car :test #'equal :from-end t))
  (when +popup-mode
    (setq display-buffer-alist +popup--display-buffer-alist)))

;;;###autoload
(defun +popup-kill-buffer-hook-h ()
  "TODO"
  (when-let (window (get-buffer-window))
    (when (+popup-window-p window)
      (let ((+popup--inhibit-transient t))
        (+popup--delete-window window)))))


;;
;; Commands

;;;###autoload
(defalias 'other-popup #'+popup/other)

;;;###autoload
(defun +popup/buffer ()
  "Open this buffer in a popup window."
  (interactive)
  (let ((+popup-default-display-buffer-actions
         '(+popup-display-buffer-stacked-side-window-fn))
        (display-buffer-alist +popup--display-buffer-alist)
        (buffer (current-buffer)))
    (push (+popup-make-rule "." +popup-defaults) display-buffer-alist)
    (bury-buffer)
    (pop-to-buffer buffer)))

;;;###autoload
(defun +popup/other ()
  "Cycle through popup windows, like `other-window'. Ignores regular windows."
  (interactive)
  (if-let* ((popups (cl-remove-if-not
                     (lambda (w) (or (+popup-window-p w)
                                     ;; This command should be able to hop between
                                     ;; windows with a `no-other-window'
                                     ;; parameter, since `other-window' won't.
                                     (window-parameter w 'no-other-window)))
                     (window-list))))
      (select-window (if (or (+popup-window-p)
                             (window-parameter nil 'no-other-window))
                         (let ((window (selected-window)))
                           (or (car-safe (cdr (memq window popups)))
                               (car (delq window popups))
                               (car popups)))
                       (car popups)))
    (user-error "No popups are open")))

;;;###autoload
(defun +popup/close (&optional window force-p)
  "Close WINDOW, if it's a popup window.

This will do nothing if the popup's `quit' window parameter is either nil or
'other. This window parameter is ignored if FORCE-P is non-nil."
  (interactive
   (list (selected-window)
         current-prefix-arg))
  (let ((window (or window (selected-window))))
    (when (and (+popup-window-p window)
               (or force-p
                   (memq (+popup-parameter-fn 'quit window window)
                         '(t current))))
      (when +popup--remember-last
        (+popup--remember (list window)))
      (delete-window window)
      t)))

;;;###autoload
(defun +popup/close-all (&optional force-p)
  "Close all open popup windows.

This will ignore popups with an `quit' parameter that is either nil or 'current.
This window parameter is ignored if FORCE-P is non-nil."
  (interactive "P")
  (let (targets +popup--remember-last)
    (dolist (window (+popup-windows))
      (when (or force-p
                (memq (+popup-parameter-fn 'quit window window)
                      '(t other)))
        (push window targets)))
    (when targets
      (+popup--remember targets)
      (mapc #'delete-window targets)
      t)))

;;;###autoload
(defun +popup/toggle ()
  "Toggle any visible popups.
If no popups are available, display the *Messages* buffer in a popup window."
  (interactive)
  (let ((+popup--inhibit-transient t))
    (cond ((+popup-windows) (+popup/close-all t))
          ((ignore-errors (+popup/restore)))
          ((display-buffer (get-buffer "*Messages*"))))))

;;;###autoload
(defun +popup/restore ()
  "Restore the last popups that were closed, if any."
  (interactive)
  (unless +popup--last
    (error "No popups to restore"))
  (cl-loop for (buffer . state) in +popup--last
           if (buffer-live-p buffer)
           do (+popup-buffer buffer (+popup-alist-from-window-state state)))
  (setq +popup--last nil)
  t)

;;;###autoload
(defun +popup/raise (window &optional arg)
  "Raise a popup WINDOW into a regular window, then select it.

When called interactively, the selected popup window will be raised. If the
selected window isn't a popup, any sole, visible popup window in the active
frame will be raised. If there are multiple visible popups, then the user will
be prompted to select one.

If prefix ARG, the popup is raised into `other-window' instead."
  (interactive
   (list
    (let ((win (selected-window)))
      (if (+popup-window-p win)
          win
        (if-let* ((popups
                   (cl-loop for w in (+popup-windows)
                            collect (cons (buffer-name (window-buffer w)) w))))
            (if (cdr popups)
                (or (cdr (assoc (completing-read
                                 "Select window: " (mapcar #'car popups))
                                popups))
                    (user-error "Aborted"))
              (cdar popups))
          (user-error "No popup windows to raise"))))
    current-prefix-arg))
  (cl-check-type window window)
  (unless (+popup-window-p window)
    (user-error "Cannot raise a non-popup window"))
  (let ((buffer (window-buffer window))
        (+popup--inhibit-transient t)
        +popup--remember-last)
    (+popup/close window 'force)
    (let (display-buffer-alist)
      (if arg
          (pop-to-buffer buffer)
        (switch-to-buffer buffer)))
    (selected-window)))

;;;###autoload
(defun +popup/diagnose ()
  "Reveal what popup rule will be used for the current buffer."
  (interactive)
  (if-let* ((rule (cl-loop with bname = (buffer-name)
                           for (pred . action) in display-buffer-alist
                           if (and (functionp pred) (funcall pred bname action))
                           return (cons pred action)
                           else if (and (stringp pred) (string-match-p pred bname))
                           return (cons pred action))))
      (message "Rule matches: %s" rule)
    (message "No popup rule for this buffer")))


;;
;;; Advice

;;;###autoload
(defun +popup-close-a (&rest _)
  "TODO"
  (+popup/close nil t))

;;;###autoload
(defun +popup-save-a (fn &rest args)
  "Sets aside all popups before executing the original function, usually to
prevent the popup(s) from messing up the UI (or vice versa)."
  (save-popups! (apply fn args)))

;;;###autoload
(defun +popup-display-buffer-fullframe-fn (buffer alist)
  "Displays the buffer fullscreen."
  (let ((wconf (current-window-configuration)))
    (when-let (window (or (display-buffer-reuse-window buffer alist)
                          (display-buffer-same-window buffer alist)
                          (display-buffer-pop-up-window buffer alist)
                          (display-buffer-use-some-window buffer alist)))
      (set-window-parameter window 'saved-wconf wconf)
      (add-to-list 'window-persistent-parameters '(saved-wconf . t))
      (delete-other-windows window)
      window)))

;;;###autoload
(defun +popup-display-buffer-stacked-side-window-fn (buffer alist)
  "A `display-buffer' action that serves as an alternative to
`display-buffer-in-side-window', but allows for stacking popups with the `vslot'
alist entry.

Accepts the same arguments as `display-buffer-in-side-window'. You must set
`window--sides-inhibit-check' to non-nil for this work properly."
  (let* ((side  (or (cdr (assq 'side alist)) 'bottom))
         (slot  (or (cdr (assq 'slot alist))  0))
         (vslot (or (cdr (assq 'vslot alist)) 0))
         (left-or-right (memq side '(left right)))
         (display-buffer-mark-dedicated (or display-buffer-mark-dedicated 'popup)))

    (cond ((not (memq side '(top bottom left right)))
           (error "Invalid side %s specified" side))
          ((not (numberp slot))
           (error "Invalid slot %s specified" slot))
          ((not (numberp vslot))
           (error "Invalid vslot %s specified" vslot)))

    (let* ((live (get-window-with-predicate
                   (lambda (window)
                     (and (eq (window-parameter window 'window-side) side)
                          (eq (window-parameter window 'window-vslot) vslot)))
                   nil))
           ;; As opposed to the `window-side' property, our `window-vslot'
           ;; parameter is set only on a single live window and never on internal
           ;; windows. Moreover, as opposed to `window-with-parameter' (as used
           ;; by the original `display-buffer-in-side-window'),
           ;; `get-window-with-predicate' only returns live windows anyway. In
           ;; any case, we will have missed the major side window and got a
           ;; child instead if the major side window happens to be an internal
           ;; window with multiple children. In that case, all childen should
           ;; have the same `window-vslot' parameter, and the major side window
           ;; is the parent of the live window.
           (prev (and live (window-prev-sibling live)))
           (next (and live (window-next-sibling live)))
           (prev-vslot (and prev (window-parameter prev 'window-vslot)))
           (next-vslot (and next (window-parameter next 'window-vslot)))
           (major (and live
                       (if (or (eq prev-vslot vslot) (eq next-vslot vslot))
                           (window-parent live)
                         live)))
           (reversed (window--sides-reverse-on-frame-p (selected-frame)))
           (windows
            (cond ((window-live-p major)
                   (list major))
                  ((window-valid-p major)
                   (let* ((first (window-child major))
                          (next (window-next-sibling first))
                          (windows (list next first)))
                     (setq reversed (> (window-parameter first 'window-slot)
                                       (window-parameter next 'window-slot)))
                     (while (setq next (window-next-sibling next))
                       (setq windows (cons next windows)))
                     (if reversed windows (nreverse windows))))))
           (slots (if major (max 1 (window-child-count major))))
           (max-slots
            (nth (plist-get '(left 0 top 1 right 2 bottom 3) side)
                 window-sides-slots))
           (window--sides-inhibit-check t)
           window this-window this-slot prev-window next-window
           best-window best-slot abs-slot)

      (cond ((and (numberp max-slots) (<= max-slots 0))
             nil)
            ((not windows)
             (cl-letf (((symbol-function 'window--make-major-side-window-next-to)
                        (lambda (_side) (frame-root-window (selected-frame)))))
               (when-let (window (window--make-major-side-window buffer side slot alist))
                 (set-window-parameter window 'window-vslot vslot)
                 (add-to-list 'window-persistent-parameters '(window-vslot . writable))
                 window)))
            (t
             ;; Scan windows on SIDE.
             (catch 'found
               (dolist (window windows)
                 (setq this-slot (window-parameter window 'window-slot))
                 (cond ((not (numberp this-slot)))
                       ((= this-slot slot) ; A window with a matching slot found
                        (setq this-window window)
                        (throw 'found t))
                       (t
                        ;; Check if this window has a better slot value wrt the
                        ;; slot of the window we want.
                        (setq abs-slot
                              (if (or (and (> this-slot 0) (> slot 0))
                                      (and (< this-slot 0) (< slot 0)))
                                  (abs (- slot this-slot))
                                (+ (abs slot) (abs this-slot))))
                        (unless (and best-slot (<= best-slot abs-slot))
                          (setq best-window window)
                          (setq best-slot abs-slot))
                        (if reversed
                            (cond
                             ((<= this-slot slot)
                              (setq next-window window))
                             ((not prev-window)
                              (setq prev-window window)))
                          (cond
                           ((<= this-slot slot)
                            (setq prev-window window))
                           ((not next-window)
                            (setq next-window window))))))))

             ;; `this-window' is the first window with the same SLOT.
             ;; `prev-window' is the window with the largest slot < SLOT. A new
             ;; window will be created after it.
             ;; `next-window' is the window with the smallest slot > SLOT. A new
             ;; window will be created before it.
             ;; `best-window' is the window with the smallest absolute
             ;; difference of its slot and SLOT.
             (or (and this-window
                      ;; Reuse `this-window'.
                      (with-current-buffer buffer
                        (setq window--sides-shown t))
                      (window--display-buffer
                       buffer this-window 'reuse alist))
                 (and (or (not max-slots) (< slots max-slots))
                      (or (and next-window
                               ;; Make new window before `next-window'.
                               (let ((next-side (if left-or-right 'above 'left))
                                     (+popup--internal t)
                                     (window-combination-resize 'side))
                                 (setq window
                                       (ignore-errors (split-window next-window nil next-side)))))
                          (and prev-window
                               ;; Make new window after `prev-window'.
                               (let ((prev-side (if left-or-right 'below 'right))
                                     (+popup--internal t)
                                     (window-combination-resize 'side))
                                 (setq window
                                       (ignore-errors (split-window prev-window nil prev-side))))))
                      (set-window-parameter window 'window-slot slot)
                      (set-window-parameter window 'window-vslot vslot)
                      (with-current-buffer buffer
                        (setq window--sides-shown t))
                      (window--display-buffer
                       buffer window 'window alist))
                 (and best-window
                      ;; Reuse `best-window'.
                      (progn
                        ;; Give best-window the new slot value.
                        (set-window-parameter best-window 'window-slot slot)
                        (with-current-buffer buffer
                          (setq window--sides-shown t))
                        (window--display-buffer
                         buffer best-window 'reuse alist)))))))))
