;;; core-popups.el --- taming sudden yet inevitable windows

;; I'd like certain buffers--like help windows, prompts or
;; informational/terminal/temporary buffers--to have less presence among my work
;; buffers (typically, source code buffers). I'd also like them to be easy to
;; both dispose of quickly and invoke from anywhere. It will also hide the
;; mode-line in popups using `doom-hide-modeline-mode'
;;
;; I use `shackle' to make this as consistent as possible, which lets you
;; specify rules on how to treat certain buffers. I go through great lengths to
;; tame helm, flycheck, help buffers--*even* the beast that is org-mode, with
;; the help of `display-buffer-alist' and `shackle'.
;;
;; Be warned, there is a lot of hackery voodoo here that could break with an
;; emacs update, or an update to any of the packages it tries to tame (like helm
;; or org-mode).

(package! shackle :demand t
  :config
  (shackle-mode 1)
  (setq shackle-default-alignment 'below
        shackle-select-reused-windows t)

  (def-setting! :popup (rule)
    "Prepend a new popup rule to `shackle-rules'."
    ;; Ensure some default attributes are set for window rules
    (let ((pattern (car rule))
          (ruleset (cdr rule)))
      ;; Align popups by default (error if this doesn't happen)
      (unless (plist-member ruleset :align)
        (plist-put ruleset :align shackle-default-alignment))
      ;; Select popups by default
      (unless (or (plist-member ruleset :select)
                  (plist-member ruleset :noselect))
        (plist-put ruleset :select t))
      (setq rule (append (list pattern) ruleset))
      `(push ',rule shackle-rules)))

  ;; :noesc and :modeline are custom settings and are not part of shackle. See
  ;; `doom*popup-init' and `doom-popup-buffer' for how they're used.
  (set! :popup
    ("^ ?\\*doom:.+\\*$" :size 35  :regexp t   :modeline t)
    ("^ ?\\*doom .+\\*$" :size 12  :noselect t :regexp t :modeline t)
    ("^\\*.+-Profiler-Report .+\\*$" :size 0.3 :regexp t)
    ("*esup*"            :size 0.4 :noselect t :noesc t)
    ("*minor-modes*"     :size 0.5 :noselect t)
    ("*eval*"            :size 16  :noselect t)
    ("*Pp Eval Output*"  :size 0.3)
    ("*Apropos*"         :size 0.3)
    ("*Backtrace*"       :size 25  :noselect t)
    ("*Help*"            :size 16)
    ("*Messages*"        :size 10  :select t)
    ("*Warnings*"        :size 10  :noselect t)
    ("*command-log*"     :size 28  :noselect t :align right)
    (compilation-mode    :size 15  :noselect t :noesc t)
    (ivy-occur-grep-mode :size 25  :noesc t)
    (eww-mode            :size 30)
    (comint-mode         :noesc t)
    (tabulated-list-mode :noesc t))

  (defvar doom-popup-history nil
    "A list of popups that were last closed. Used by `doom/popup-restore' and
`doom*popup-save'.")

  (defvar doom-popup-remember-history t
    "If non-nil, DOOM will remember the last popup(s) that were open in
`doom-popup-history'.")

  (defvar doom-popup-other-window nil
    "The last window selected before a popup was opened.")

  (defvar-local doom-popup-rules nil
    "The shackle rule that caused this buffer to be recognized as a popup.")

  (defvar doom-popup-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map [escape]    'doom/popup-close-maybe)
      (define-key map (kbd "ESC") 'doom/popup-close-maybe)
      map)
    "Active keymap in popup windows.")

  (define-minor-mode doom-popup-mode
    "Minor mode for pop-up windows."
    :init-value nil
    :keymap doom-popup-mode-map
    (if (and (not doom-popup-mode)
             doom-hide-modeline-mode)
        (doom-hide-modeline-mode -1)
      (let ((modeline (plist-get doom-popup-rules :modeline)))
        (cond ((eq modeline 'nil)
               (doom-hide-modeline-mode +1))
              ((symbolp modeline)
               (let ((doom--hidden-modeline-format (+doom-modeline modeline)))
                 (doom-hide-modeline-mode +1))))))
    (set-window-dedicated-p nil doom-popup-mode))
  (put 'doom-popup-mode 'permanent-local t)

  ;; Tell `window-state-get' and `current-window-configuration' to persist these
  ;; custom parameters.
  (dolist (param '(popup noesc))
    (add-to-list 'window-persistent-parameters (cons param 'writable)))

  (defun doom*popup-init (orig-fn &rest args)
    "Enables `doom-popup-mode' in popup windows and returns the window."
    (unless (doom-popup-p)
      (setq doom-popup-other-window (selected-window)))
    (let ((window (apply orig-fn args))
          (rules (nth 2 args)))
      (unless window
        (error "No window was found (%s)" args))
      (mapc (lambda (cfg) (set-window-parameter window (car cfg) (cdr cfg)))
            (append `((popup . ,rules)
                      (no-other-window . ,t))
                    (when (plist-get rules :noesc)
                      `((noesc . ,t)))))
      (with-selected-window window
        (setq-local doom-popup-rules rules)
        (doom-popup-mode +1))
      ;; NOTE orig-fn returns a window, so `doom*popup-init' must too
      window))

  (defun doom*popup-save (orig-fn &rest args)
    "Puts aside all popups before executing the original function, usually to
prevent the popups from interfering (or the other way around)."
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

  ;; There is no shackle-popup hook, so I created one:
  (advice-add 'shackle-display-buffer :around 'doom*popup-init)
  ;; Don't affect popup windows
  (advice-add 'balance-windows :around 'doom*popup-save))


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

  ;; Close popups when you press ESC in normal mode, in any buffer
  (defun doom*popup-evil-close-on-esc ()
    "Close non-repl popups and clean up `doom-popup-windows'."
    (unless (or (minibuffer-window-active-p (minibuffer-window))
                (evil-ex-hl-active-p 'evil-ex-search))
      (doom/popup-close-all)))
  (advice-add 'evil-force-normal-state :after 'doom*popup-evil-close-on-esc)

  ;; Tame the command window
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
  (advice-add 'evil-command-window :override 'doom*popup-evil-command-window)

  (defun doom*popup-evil-command-window-execute ()
    "Execute the command under the cursor in the appropriate buffer."
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
  (advice-add 'evil-command-window-execute :override 'doom*popup-evil-command-window-execute)

  ;; Tell these functions not to mess with popups
  (advice-add 'doom-evil-window-move        :around 'doom*popup-save)
  (advice-add 'evil-window-move-very-bottom :around 'doom*popup-save)
  (advice-add 'evil-window-move-very-top    :around 'doom*popup-save)
  (advice-add 'evil-window-move-far-left    :around 'doom*popup-save)
  (advice-add 'evil-window-move-far-right   :around 'doom*popup-save)

  ;; Don't block moving to/from popup windows
  (defun doom*ignore-window-parameters-in-popups (dir &optional arg window)
    (window-in-direction
     (cond ((eq dir 'up) 'above)
           ((eq dir 'down) 'below)
           (t dir))
     window t arg windmove-wrap-around t))
  (advice-add 'windmove-find-other-window :override 'doom*ignore-window-parameters-in-popups))

;; (after! help-mode
;;   ;; Help buffers use itself (or `other-window') to decide where to open
;;   ;; followed links, which can be unpredictable. It should *only* replace the
;;   ;; original buffer we opened the popup from. To fix this these three button
;;   ;; types need to be redefined to set aside the popup before following a link.
;;   (define-button-type 'help-function-def
;;     :supertype 'help-xref
;;     'help-function (lambda (fun file)
;;                      (require 'find-func)
;;                      (when (eq file 'C-source)
;;                        (setq file (help-C-file-name (indirect-function fun) 'fun)))
;;                      (let ((location (find-function-search-for-symbol fun nil file)))
;;                        (doom/popup-close)
;;                        (switch-to-buffer (car location) nil t)
;;                        (if (cdr location)
;;                            (progn
;;                              (goto-char (cdr location))
;;                              (recenter nil))
;;                          (message "Unable to find location in file")))))

;;   (define-button-type 'help-variable-def
;;     :supertype 'help-xref
;;     'help-function (lambda (var &optional file)
;;                      (when (eq file 'C-source)
;;                        (setq file (help-C-file-name var 'var)))
;;                      (let ((location (find-variable-noselect var file)))
;;                        (doom/popup-close)
;;                        (switch-to-buffer (car location) nil t)
;;                        (if (cdr location)
;;                            (progn
;;                              (goto-char (cdr location))
;;                              (recenter nil))
;;                          (message "Unable to find location in file")))))

;;   (define-button-type 'help-face-def
;;     :supertype 'help-xref
;;     'help-function (lambda (fun file)
;;                      (require 'find-func)
;;                      (let ((location
;;                             (find-function-search-for-symbol fun 'defface file)))
;;                        (doom/popup-close)
;;                        (switch-to-buffer (car location) nil t)
;;                        (if (cdr location)
;;                            (progn
;;                              (goto-char (cdr location))
;;                              (recenter nil))
;;                          (message "Unable to find location in file"))))))

;; (after! magit
;;   ;; Don't open files (from magit) within the magit popup
;;   (advice-add 'magit-display-file-buffer-traditional :around 'doom*popup-save))

(after! neotree
  (defun doom*popup-save-neotree (orig-fun &rest args)
    "Prevents messing up the neotree buffer on window changes."
    (let ((neo-p (and (featurep 'neotree) (neo-global--window-exists-p))))
      (when neo-p
        (neotree-hide)
        (balance-windows))
      (unwind-protect (apply orig-fun args)
        (when neo-p
          (save-selected-window
            (neotree-show))))))

  ;; Prevent neotree from interfering with popups
  (advice-add 'shackle-display-buffer :around 'doom*popup-save-neotree)
  ;; Prevents messing up the neotree buffer on window changes
  (advice-add 'doom-evil-window-move :around 'doom*popup-save-neotree)
  ;; (advice-add 'doom-popup-buffer     :around 'doom*popup-save-neotree)
  ;; Don't let neotree interfere with moving, splitting or rebalancing windows
  (advice-add 'balance-windows :around 'doom*popup-save-neotree)
  (advice-add 'split-window    :around 'doom*popup-save-neotree)
  (advice-add 'shackle-display-buffer :around 'doom*popup-save-neotree)
  (advice-add 'evil-window-move-very-bottom :around 'doom*popup-save-neotree)
  (advice-add 'evil-window-move-very-top    :around 'doom*popup-save-neotree)
  (advice-add 'evil-window-move-far-left    :around 'doom*popup-save-neotree)
  (advice-add 'evil-window-move-far-right   :around 'doom*popup-save-neotree))

(add-hook! org-load
  ;; Ensures org-src-edit yields control of its buffer to shackle.
  (defun doom*org-src-switch-to-buffer (buffer context) (pop-to-buffer buffer))
  (advice-add 'org-src-switch-to-buffer :override 'doom*org-src-switch-to-buffer)

  ;; ...for org-todo, org-link and org-agenda popups
  (defun doom*org-pop-to-buffer-same-window (&optional buffer-or-name norecord label)
    "Pop to buffer specified by BUFFER-OR-NAME in the selected window."
    (display-buffer buffer-or-name))
  (advice-add 'org-pop-to-buffer-same-window :override 'doom*org-pop-to-buffer-same-window)

  (defun doom*org-switch-to-buffer-other-window (&rest args)
    (car-safe
     (mapc (lambda (b)
             (let ((buf (if (stringp b) (get-buffer-create b) b)))
               (pop-to-buffer buf t t)))
           args)))
  (advice-add 'org-switch-to-buffer-other-window :override 'doom*org-switch-to-buffer-other-window)

  (defun doom/popup-org-agenda-quit ()
    "Necessary to finagle org-agenda into shackle popups & behave on quit."
    (interactive)
    (if org-agenda-columns-active
        (org-columns-quit)
      (let ((buf (current-buffer)))
        (and (not (eq org-agenda-window-setup 'current-window))
             (not (one-window-p))
             (delete-window))
        (kill-buffer buf)
        (setq org-agenda-archives-mode nil
              org-agenda-buffer nil))))

  (after! org-agenda
    (after! evil
      (evil-define-key* 'motion org-agenda-mode-map
        [escape] 'doom/popup-org-agenda-quit
        (kbd "ESC") 'doom/popup-org-agenda-quit))

    (let ((map org-agenda-mode-map))
      (define-key map "q" 'doom/popup-org-agenda-quit)
      (define-key map "Q" 'doom/popup-org-agenda-quit))))

(after! repl-toggle
  (add-hook! doom-popup-close
    (setq rtog/--last-buffer nil)))

(provide 'core-popups)
;;; core-popups.el ends here
