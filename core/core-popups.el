;;; core-popups.el --- taming sudden yet inevitable windows

;; I want a "real"-buffer-first policy in my Emacsian utpoia; popup buffers
;; ought to be second-class citizens to "real" buffers. No need for a wall or
;; controversial immigration policies -- all we need is `shackle'.
;;
;; The gist is: popups should always be displayed on one side of the frame, away
;; from 'real' buffers; they should be easy to dispose of when we don't want to
;; see them; and easily brought back in case we change our minds. Also, popups
;; should typically have no mode-line.
;;
;; Be warned, this requires a lot of hackery and voodoo that could break with an
;; emacs update or an update to any of the packages it tries to tame (like helm
;; or org-mode).

(defvar doom-popup-history nil
  "A list of popups that were last closed. Used by `doom/popup-restore' and
`doom*popups-save'.")

(defvar doom-popup-remember-history t
  "If non-nil, DOOM will remember the last popup(s) that were open in
`doom-popup-history'.")

(defvar doom-popup-other-window nil
  "The last window selected before a popup was opened.")

(defvar-local doom-popup-rules nil
  "The shackle rule that caused this buffer to be recognized as a popup.")

(defvar doom-popup-window-parameters '(:noesc :modeline :autokill)
  "A list of window parameters that are set (and cleared) when `doom-popup-mode
is enabled/disabled.'")

(def-setting! :popup (&rest rules)
  "Prepend a new popup rule to `shackle-rules'."
  (if (cl-every 'listp rules)
      `(nconc shackle-rules ',rules)
    `(push ',rules shackle-rules)))


;;
;; Bootstrap
;;

(def-package! shackle :demand t
  :init
  (setq shackle-default-alignment 'below
        ;;; Baseline popup-window rules
        ;; :noesc, :modeline and :autokill are custom settings and are not part
        ;; of shackle:
        ;;  :noesc      determines if pressing ESC in this popup will close it.
        ;;              Used by `doom/popup-close-maybe'.
        ;;  :modeline   By default, mode-lines are hidden in popups unless this is
        ;;              non-nil. If it is a symbol, it'll use `doom-modeline' to
        ;;              fetch a modeline config. Set in `doom-popup-mode'.
        ;;  :autokill   If non-nil, the buffer in these popups will be killed when
        ;;              their popup is closed. Used in `doom*delete-popup-window'
        shackle-rules
        '(("^ ?\\*doom:.+\\*$"      :size 40  :modeline t :regexp t)
          ("^ ?\\*doom .+\\*$"      :size 30  :noselect t :regexp t)
          ("^\\*.+-Profiler-Report .+\\*$" :size 0.3 :regexp t :autokill t)
          ("*minor-modes*"          :size 0.5 :noselect t :autokill t)
          ("*eval*"                 :size 16  :noselect t :autokill t)
          ("*Pp Eval Output*"       :size 0.3 :autokill t)
          ("*Apropos*"              :size 0.3)
          ("*Backtrace*"            :size 25  :noselect t)
          ("*Help*"                 :size 16)
          ("*Messages*"             :size 10)
          ("*Warnings*"             :size 10  :noselect t :autokill t)
          ("*command-log*"          :size 28  :noselect t :align right)
          ("*Shell Command Output*" :size 20  :noselect t :autokill t)
          ("*Occur*"                :size 25  :noselect t :autokill t)
          (compilation-mode         :size 15  :noselect t :noesc t :autokill t)
          (eww-mode                 :size 30)
          (comint-mode              :noesc t)
          (tabulated-list-mode      :noesc t)))

  :config
  (shackle-mode 1)

  (defun doom*shackle-always-align (plist)
    "Ensure popups are always aligned and selected by default. Eliminates the need
for :align t on every rule."
    (when plist
      (unless (or (plist-member plist :align)
                  (plist-member plist :same)
                  (plist-member plist :frame))
        (plist-put plist :align t))
      (unless (or (plist-member plist :select)
                  (plist-member plist :noselect))
        (plist-put plist :select t)))
    plist)
  (advice-add 'shackle--match :filter-return 'doom*shackle-always-align))


;;
;; Integration
;;

;; Tell `window-state-get' and `current-window-configuration' to recognize these
;; custom parameters. Helpful for `persp-mode' and persisting window configs
;; that have popups in them.
(push (cons 'no-other-window 'writable) window-persistent-parameters)
(dolist (param doom-popup-window-parameters)
  (push (cons param 'writable) window-persistent-parameters))

(defvar doom-popup-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [escape]    'doom/popup-close-maybe)
    (define-key map (kbd "ESC") 'doom/popup-close-maybe)
    map)
  "Active keymap in popup windows.")

(define-minor-mode doom-popup-mode
  "Minor mode for popup windows."
  :init-value nil
  :keymap doom-popup-mode-map
  (let ((window (selected-window)))
    ;; Major mode changes (and other things) may call
    ;; `kill-all-local-variables', turning off things like `doom-popup-mode'.
    ;; This prevents that.
    (put 'doom-popup-mode 'permanent-local doom-popup-mode)
    ;; Ensure that buffer-opening functions/commands (like
    ;; `switch-to-buffer-other-window' won't use this window).
    (set-window-parameter window 'no-other-window doom-popup-mode)
    ;; Makes popup window resist interactively changing its buffer.
    (set-window-dedicated-p window doom-popup-mode)
    (cond (doom-popup-mode
           ;; Don't show modeline in popup windows without a :modeline rule. If
           ;; one exists and it's a symbol, use `doom-modeline' to grab the
           ;; format. If non-nil, show the mode-line as normal. If nil (or
           ;; omitted, by default), then hide the modeline entirely.
           (let ((modeline (plist-get doom-popup-rules :modeline)))
             (cond ((or (eq modeline 'nil)
                        (not modeline))
                    (doom-hide-modeline-mode +1))
                   ((symbolp modeline)
                    (let ((doom--hidden-modeline-format (doom-modeline modeline)))
                      (doom-hide-modeline-mode +1)))))
           ;; Save metadata into window parameters so it can be saved by window
           ;; config persisting plugins like workgroups or persp-mode.
           (set-window-parameter window 'popup (or doom-popup-rules t))
           (when doom-popup-rules
             (dolist (param doom-popup-window-parameters)
               (when-let (val (plist-get doom-popup-rules param))
                 (set-window-parameter window param val)))))

          (t
           ;; show modeline
           (when doom-hide-modeline-mode
             (doom-hide-modeline-mode -1))
           ;; Ensure window parameters are cleaned up
           (set-window-parameter window 'popup nil)
           (dolist (param doom-popup-window-parameters)
             (set-window-parameter window param nil))))))

;; Hide modeline in completion popups
(add-hook! (completion-in-region-mode completion-list-mode) 'doom-hide-modeline-mode)

;;
(defun doom*popup-init (orig-fn &rest args)
  "Initializes a window as a popup window by enabling `doom-popup-mode' in it
and setting `doom-popup-rules' within it. Returns the window."
  (unless (doom-popup-p)
    (setq doom-popup-other-window (selected-window)))
  (let ((plist (or (nth 2 args)
                   (and (bufferp (car args))
                        (shackle-match (window-buffer (car args))))))
        (window (apply orig-fn args)))
    (unless window
      (error "No popup window was found for %s: %s" (car args) plist))
    (with-selected-window window
      (unless (eq plist t)
        (setq-local doom-popup-rules plist))
      (doom-popup-mode +1))
    window))

(defun doom*popups-save (orig-fn &rest args)
  "Sets aside all popups before executing the original function, usually to
prevent the popup(s) from messing up the UI (or vice versa)."
  (let ((in-popup-p (doom-popup-p))
        (popups (doom-popup-windows))
        (doom-popup-remember-history t))
    (when popups
      (mapc 'doom/popup-close popups))
    (unwind-protect (apply orig-fn args)
      (when popups
        (let ((origin (selected-window)))
          (doom/popup-restore)
          (unless in-popup-p
            (select-window origin)))))))

(defun doom*delete-popup-window (&optional window)
  "Ensure that popups are deleted properly, and killed if they have :autokill
properties."
  (let ((window (or window (selected-window))))
    (when (doom-popup-p window)
      (when doom-popup-remember-history
        (setq doom-popup-history (list (doom--popup-data window))))
      (let ((autokill-p (window-parameter window :autokill)))
        (with-selected-window window
          (doom-popup-mode -1)
          (when autokill-p
            (kill-buffer (current-buffer))))))))

(advice-add 'shackle-display-buffer :around 'doom*popup-init)
(advice-add 'balance-windows :around 'doom*popups-save)
(advice-add 'delete-window :before 'doom*delete-popup-window)


;;
;; Hacks
;;

(after! evil
  (let ((map doom-popup-mode-map))
    (define-key map [remap evil-window-delete]           'doom/popup-close)
    (define-key map [remap evil-window-move-very-bottom] 'ignore)
    (define-key map [remap evil-window-move-very-top]    'ignore)
    (define-key map [remap evil-window-move-far-left]    'ignore)
    (define-key map [remap evil-window-move-far-right]   'ignore)
    (define-key map [remap evil-window-split]            'ignore)
    (define-key map [remap evil-window-vsplit]           'ignore)
    (define-key map [remap evil-force-normal-state]      'doom/popup-close-maybe))

  ;; Make evil-mode cooperate with popups
  (advice-add 'evil-command-window :override 'doom*popup-evil-command-window)
  (advice-add 'evil-command-window-execute :override 'doom*popup-evil-command-window-execute)

  (defun doom*popup-evil-command-window (hist cmd-key execute-fn)
    "The evil command window has a mind of its own (uses `switch-to-buffer'). We
monkey patch it to use pop-to-buffer, and to remember the previous window."
    (when (eq major-mode 'evil-command-window-mode)
      (user-error "Cannot recursively open command line window"))
    (dolist (win (window-list))
      (when (equal (buffer-name (window-buffer win))
                   "*Command Line*")
        (kill-buffer (window-buffer win))
        (delete-window win)))
    (setq evil-command-window-current-buffer (current-buffer))
    (ignore-errors (kill-buffer "*Command Line*"))
    (with-current-buffer (pop-to-buffer "*Command Line*")
      (setq-local evil-command-window-execute-fn execute-fn)
      (setq-local evil-command-window-cmd-key cmd-key)
      (evil-command-window-mode)
      (evil-command-window-insert-commands hist)))

  (defun doom*popup-evil-command-window-execute ()
    "Execute the command under the cursor in the appropriate buffer, rather than
the command buffer."
    (interactive)
    (let ((result (buffer-substring (line-beginning-position)
                                    (line-end-position)))
          (execute-fn evil-command-window-execute-fn)
          (popup (selected-window)))
      (select-window doom-popup-other-window)
      (unless (equal evil-command-window-current-buffer (current-buffer))
        (user-error "Originating buffer is no longer active"))
      ;; (kill-buffer "*Command Line*")
      (doom/popup-close popup)
      (funcall execute-fn result)
      (setq evil-command-window-current-buffer nil)))

  ;; Don't mess with popups
  (advice-add 'doom-evil-window-move        :around 'doom*popups-save)
  (advice-add 'evil-window-move-very-bottom :around 'doom*popups-save)
  (advice-add 'evil-window-move-very-top    :around 'doom*popups-save)
  (advice-add 'evil-window-move-far-left    :around 'doom*popups-save)
  (advice-add 'evil-window-move-far-right   :around 'doom*popups-save)

  ;; Don't block moving to/from popup windows
  (defun doom*ignore-window-parameters-in-popups (dir &optional arg window)
    (window-in-direction (cond ((eq dir 'up)   'above)
                               ((eq dir 'down) 'below)
                               (t dir))
                         window t arg windmove-wrap-around t))
  (advice-add 'windmove-find-other-window :override 'doom*ignore-window-parameters-in-popups))


(after! help-mode
  ;; Help buffers use `other-window' to decide where to open followed links,
  ;; which can be unpredictable. It should *only* replace the original buffer we
  ;; opened the popup from. To fix this these three button types need to be
  ;; redefined to set aside the popup before following a link.
  (defsubst doom--switch-from-popup (location)
    (doom/popup-close)
    (switch-to-buffer (car location) nil t)
    (if (not (cdr location))
        (message "Unable to find location in file")
      (goto-char (cdr location))
      (recenter)))

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


;; (after! magit
;;   ;; Don't open files (from magit) within the magit popup
;;   (advice-add 'magit-display-file-buffer-traditional :around 'doom*popups-save))


(after! neotree
  (defun doom*popups-save-neotree (orig-fn &rest args)
    "Prevents messing up the neotree buffer on window changes."
    (let ((neo-p (and (featurep 'neotree)
                      (neo-global--window-exists-p))))
      (when neo-p
        (neotree-hide))
      (unwind-protect (apply orig-fn args)
        (when neo-p
          (save-selected-window
            (neotree-show))))))

  ;; Prevents messing up the neotree buffer on window changes
  (advice-add '+evil-window-move :around 'doom*popups-save-neotree)
  ;; Don't let neotree interfere with moving, splitting or rebalancing windows
  (advice-add 'evil-window-move-very-bottom :around 'doom*popups-save-neotree)
  (advice-add 'evil-window-move-very-top    :around 'doom*popups-save-neotree)
  (advice-add 'evil-window-move-far-left    :around 'doom*popups-save-neotree)
  (advice-add 'evil-window-move-far-right   :around 'doom*popups-save-neotree))


(add-hook! org-load
  (set! :popup
    '("*Calendar*"         :size 0.4 :noselect t)
    '(" *Org todo*"        :size 5 :noselect t)
    '("*Org Note*"         :size 10)
    '("*Org Select*"       :size 20 :noselect t)
    '("*Org Links*"        :size 5 :noselect t)
    '(" *Agenda Commands*" :noselect t)
    '("^\\*Org Agenda"     :regexp t :size 0.4)
    '("*Org Clock*"        :noselect t)
    '("*Edit Formulas*"    :size 10)
    '("\\*Org Src"         :regexp t :size 15)
    '("^\\*Org-Babel"      :regexp t :size 0.4)
    '("^CAPTURE.*\\.org$"  :regexp t :size 20))

  ;; Org tries to do its own popup management, causing buffer/window config
  ;; armageddon when paired with shackle. To fix this, first we suppress
  ;; delete-other-windows in org functions:
  (defun doom*suppress-delete-other-windows (orig-fn &rest args)
    (cl-flet ((silence (&rest args) (ignore)))
      (advice-add 'delete-other-windows :around #'silence)
      (unwind-protect
          (apply orig-fn args)
        (advice-remove 'delete-other-windows #'silence))))
  (advice-add 'org-capture-place-template :around #'doom*suppress-delete-other-windows)
  (advice-add 'org-agenda :around #'doom*suppress-delete-other-windows)
  (advice-add 'org-add-log-note :around #'doom*suppress-delete-other-windows)

  ;; Tell org-src-edit to open another window, which shackle can intercept.
  (setq org-src-window-setup 'other-window)

  ;; Then, we tell org functions to use pop-to-buffer instead of
  ;; switch-to-buffer-*. Buffers get handed off to shackle properly this way.
  (defun doom*org-switch-to-buffer-other-window (&rest args)
    (pop-to-buffer (car args)))
  (advice-add 'org-switch-to-buffer-other-window :override 'doom*org-switch-to-buffer-other-window)

  ;; Hide modeline in org-agenda
  (add-hook 'org-agenda-finalize-hook 'doom-hide-modeline-mode)

  (after! org-agenda
    (after! evil
      (map! :map* org-agenda-mode-map
            :m [escape] 'org-agenda-Quit
            :m "ESC"    'org-agenda-Quit))
    (let ((map org-agenda-mode-map))
      (define-key map "q" 'org-agenda-Quit)
      (define-key map "Q" 'org-agenda-Quit))))


(after! repl-toggle
  (add-hook! doom-popup-close
    (setq rtog/--last-buffer nil)))

(provide 'core-popups)
;;; core-popups.el ends here
