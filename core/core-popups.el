;;; core-popups.el --- taming sudden yet inevitable windows

;; I'd like certain buffers--like help windows, prompts or
;; informational/terminal/temporary buffers--to have less presence over my work
;; buffers (e.g. source code buffers). I'd also like them to be easy to both
;; dispose of quickly and invoke from anywhere. Also, hide the mode-line in
;; popups with `doom-hide-modeline-mode'
;;
;; I use `shackle' to make this as consistent as possible, which allows you
;; to specify rules on how to treat certain buffers.
;;
;; Be warned, there is a lot of hackery voodoo here that could break with an
;; emacs update, or an update to any of the packages it tries to tame (like helm
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

(defvar doom-popup-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [escape]    'doom/popup-close-maybe)
    (define-key map (kbd "ESC") 'doom/popup-close-maybe)
    map)
  "Active keymap in popup windows.")

(@def-setting :popup (&rest rules)
  "Prepend a new popup rule to `shackle-rules'."
  (if (cl-every 'listp rules)
      `(nconc shackle-rules ',rules)
    `(push ',rules shackle-rules)))


;;
;; Bootstrap
;;

(@def-package shackle :demand t
  :init
  (setq shackle-default-alignment 'below
        ;;; Baseline popup-window rules
        ;; :noesc, :modeline and :autokill are custom settings and are not part
        ;; of shackle. See `doom*popup-init' and `doom-popup-buffer' for how
        ;; they're used.
        shackle-rules
        '(("^ ?\\*doom:.+\\*$"      :size 40  :modeline t :regexp t)
          ("^ ?\\*doom .+\\*$"      :size 30  :noselect t :regexp t)
          ("^\\*.+-Profiler-Report .+\\*$" :size 0.3 :regexp t :autokill t)
          ("*minor-modes*"          :size 0.5 :noselect t :autokill t)
          ("*eval*"                 :size 16  :noselect t :autokill t)
          ("*Pp Eval Output*"       :size 0.3 :autokill t)
          ("*Apropos*"              :size 0.3)
          ("*Backtrace*"            :size 25  :noselect t)
          ("*Help*"                 :size 16  :autokill t)
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
  (shackle-mode 1))


;;
;; Modifications
;;

(defun doom*shackle-always-align (plist)
  "Ensure popups are always aligned and selected by default. Eliminates the need
for :align t on every rule."
  (when plist
    (unless (plist-member plist :align)
      (plist-put plist :align t))
    (unless (or (plist-member plist :select)
                (plist-member plist :noselect))
      (plist-put plist :select t)))
  plist)
(advice-add 'shackle--match :filter-return 'doom*shackle-always-align)


;; Tell `window-state-get' and `current-window-configuration' to persist these
;; custom parameters. Allows `persp-mode' to remember popup states.
(nconc window-persistent-parameters
       '((popup . writable)
         (noesc . writable)
         (autokill . writable)))


(define-minor-mode doom-popup-mode
  "Minor mode for popup windows."
  :init-value nil
  :keymap doom-popup-mode-map
  ;; Don't show modeline in popup windows without a :modeline rule. If one
  ;; exists and it's a symbol, use `doom-modeline' to grab the format. If
  ;; non-nil, show the mode-line as normal. If nil (or omitted), then hide the
  ;; modeline entirely.
  (if (and (not doom-popup-mode)
           doom-hide-modeline-mode)
      (doom-hide-modeline-mode -1)
    (let ((modeline (plist-get doom-popup-rules :modeline)))
      (cond ((or (eq modeline 'nil)
                 (not modeline))
             (doom-hide-modeline-mode +1))
            ((symbolp modeline)
             (let ((doom--hidden-modeline-format (doom-modeline modeline)))
               (doom-hide-modeline-mode +1)))))))
(put 'doom-popup-mode 'permanent-local t)

;; Hide modeline in completion popups
(@add-hook (completion-in-region-mode completion-list-mode) 'doom-hide-modeline-mode)

(defun doom-popup--init (window &optional plist)
  "Initializes a window as a popup window. Sets custom window parameters and
enables `doom-popup-mode'."
  (unless window
    (error "No window was found for %s: %s" (car args) plist))
  (unless plist
    (setq plist (shackle-match (window-buffer window))))
  (mapc (lambda (cfg) (set-window-parameter window (car cfg) (cdr cfg)))
        (append `((popup . ,plist)
                  (no-other-window . ,t))
                (when (plist-get plist :noesc)
                  `((noesc . ,t)))
                (when (plist-get plist :autokill)
                  `((autokill . ,t)))))
  (with-selected-window window
    (unless (eq plist t)
      (setq-local doom-popup-rules plist))
    (doom-popup-mode +1))
  window)

;;
(defun doom*popup-init (orig-fn &rest args)
  "Invokes `doom-popup--init' on windows that qualify as popups. Returns the window."
  (unless (doom-popup-p)
    (setq doom-popup-other-window (selected-window)))
  (doom-popup--init (apply orig-fn args) (nth 2 args)))

(defun doom*popups-save (orig-fn &rest args)
  "Puts aside all popups before executing the original function, usually to
prevent popups from messaging up the UI (or vice versa)."
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

(defun doom*delete-popup-window (orig-fn &rest args)
  "Ensure that popups are deleted properly, and killed if they have :autokill
properties."
  (let ((window (car args)))
    (when (doom-popup-p window)
      (with-selected-window window
        (doom-popup-mode -1)
        (unless doom-popup-mode
          (mapc (lambda (cfg) (set-window-parameter window cfg nil))
                '(popup no-other-window noesc autokill)))
          (set-window-dedicated-p window nil)
        (when doom-popup-remember-history
          (setq doom-popup-history (list (doom--popup-data window))))
        (when (window-parameter window 'autokill)
          (kill-buffer (window-buffer window))))))
  (apply orig-fn args))

(advice-add 'shackle-display-buffer :around 'doom*popup-init)
(advice-add 'balance-windows :around 'doom*popups-save)
(advice-add 'delete-window :around 'doom*delete-popup-window)


;;
;; Hacks
;;

(@after evil
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
    (window-in-direction
     (cond ((eq dir 'up) 'above)
           ((eq dir 'down) 'below)
           (t dir))
     window t arg windmove-wrap-around t))
  (advice-add 'windmove-find-other-window :override 'doom*ignore-window-parameters-in-popups))


(@after help-mode
  ;; Help buffers use `other-window' to decide where to open followed links,
  ;; which can be unpredictable. It should *only* replace the original buffer we
  ;; opened the popup from. To fix this these three button
  ;; types need to be redefined to set aside the popup before following a link.
  (define-button-type 'help-function-def
    :supertype 'help-xref
    'help-function (lambda (fun file)
                     (require 'find-func)
                     (when (eq file 'C-source)
                       (setq file (help-C-file-name (indirect-function fun) 'fun)))
                     (let ((location (find-function-search-for-symbol fun nil file)))
                       (doom/popup-close)
                       (switch-to-buffer (car location) nil t)
                       (if (cdr location)
                           (progn
                             (goto-char (cdr location))
                             (recenter))
                         (message "Unable to find location in file")))))

  (define-button-type 'help-variable-def
    :supertype 'help-xref
    'help-function (lambda (var &optional file)
                     (when (eq file 'C-source)
                       (setq file (help-C-file-name var 'var)))
                     (let ((location (find-variable-noselect var file)))
                       (doom/popup-close)
                       (switch-to-buffer (car location) nil t)
                       (if (cdr location)
                           (progn
                             (goto-char (cdr location))
                             (recenter))
                         (message "Unable to find location in file")))))

  (define-button-type 'help-face-def
    :supertype 'help-xref
    'help-function (lambda (fun file)
                     (require 'find-func)
                     (let ((location
                            (find-function-search-for-symbol fun 'defface file)))
                       (doom/popup-close)
                       (switch-to-buffer (car location) nil t)
                       (if (cdr location)
                           (progn
                             (goto-char (cdr location))
                             (recenter))
                         (message "Unable to find location in file"))))))


;; (@after magit
;;   ;; Don't open files (from magit) within the magit popup
;;   (advice-add 'magit-display-file-buffer-traditional :around 'doom*popups-save))


(@after neotree
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

  ;; Prevent neotree from interfering with popups
  ;; (advice-add 'shackle-display-buffer :around 'doom*popups-save-neotree)
  ;; Prevents messing up the neotree buffer on window changes
  (advice-add '+evil-window-move :around 'doom*popups-save-neotree)
  ;; (advice-add 'doom-popup-buffer     :around 'doom*popups-save-neotree)
  ;; Don't let neotree interfere with moving, splitting or rebalancing windows
  ;; (advice-add 'balance-windows :around 'doom*popups-save-neotree)
  ;; (advice-add 'split-window    :around 'doom*popups-save-neotree)
  ;; (advice-add 'shackle-display-buffer :around 'doom*popups-save-neotree)
  (advice-add 'evil-window-move-very-bottom :around 'doom*popups-save-neotree)
  (advice-add 'evil-window-move-very-top    :around 'doom*popups-save-neotree)
  (advice-add 'evil-window-move-far-left    :around 'doom*popups-save-neotree)
  (advice-add 'evil-window-move-far-right   :around 'doom*popups-save-neotree))


(@add-hook org-load
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

  (@after org-agenda
    (@after evil
      (evil-define-key* 'motion org-agenda-mode-map
        [escape] 'doom/popup-org-agenda-quit
        (kbd "ESC") 'doom/popup-org-agenda-quit))

    (let ((map org-agenda-mode-map))
      (define-key map "q" 'doom/popup-org-agenda-quit)
      (define-key map "Q" 'doom/popup-org-agenda-quit))))


(@after repl-toggle
  (@add-hook doom-popup-close
    (setq rtog/--last-buffer nil)))

(provide 'core-popups)
;;; core-popups.el ends here
