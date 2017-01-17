;;; core-popups.el --- taming sudden yet inevitable windows

;; I'd like certain buffers--like prompts or informational/terminal/temporary
;; buffers--to act independently from my work buffers, to minimize the context
;; switch between them. To do this, I relegate them to disposable "popup
;; windows" that can be invoked from anywhere.
;;
;; I use a slew of hackery to get Emacs to treat these popups consistently. It
;; goes through great lengths to tame helm, flycheck, help buffers--*even* the
;; beast that is org-mode, with the help of `display-buffer-alist' and
;; `shackle'.
;;
;; Be warned, this could break.

(package! shackle :demand t
  :config
  (shackle-mode 1)
  (setq shackle-default-alignment 'below
        shackle-rules
        `(;; Doom
          (" *doom*"           :size 35 :select t)
          ("^ ?\\*doom:.+\\*$" :size 35 :select t :regexp t)
          ("^ ?\\*doom.+\\*$"  :size 12 :noselect t :regexp t)
          ("^\\*.+-Profiler-Report .+\\*$" :size 0.3 :regexp t)
          ("*esup*"            :size 0.4 :noselect t)
          ("*minor-modes*"     :size 0.5 :noselect t)
          ("*eval*"            :size 16  :noselect t)
          ;; Emacs
          ("*Pp Eval Output*"  :size 0.3)
          ("*Apropos*"         :size 0.3)
          ("*Backtrace*"       :size 25 :noselect t)
          ("*Help*"            :size 16 :select t)
          ("*Messages*"        :size 15 :select t)
          ("*Warnings*"        :size 10 :noselect t)
          (compilation-mode    :size 15 :noselect t)
          (eww-mode            :size 30 :select t)
          ("*command-log*"     :size 28 :noselect t :align right)
          ;; evil
          ("*evil-registers*"  :size 0.3)
          ("*Command Line*"    :size 8 :select t)
          ;; git-gutter
          ("^\\*git-gutter.+\\*$" :regexp t :size 15 :noselect t)
          ;; vcs
          ("*vc-diff*"         :size 15 :noselect t)
          ("*vc-change-log*"   :size 15 :select t)
          (vc-annotate-mode    :same t)
          ))

  ;; :noesc  = Can't be closed with a single ESC
  ;; :nokill = Won't be killed when closed (only buried)
  (defvar doom-popup-rules
    '(("^\\*doom\\(:scratch\\)?\\*$" :noesc :nokill)
      ("^\\*doom.*\\*$"       :noesc :nokill)
      (ivy-occur-grep-mode    :noesc)
      (compilation-mode       :noesc)
      (comint-mode            :noesc :nokill)
      (eshell-mode            :noesc :nokill)
      (messages-buffer-mode          :nokill)
      (esup-mode              :noesc)
      (tabulated-list-mode    :noesc)))

  (defvar-local doom-popup-rule nil
    "A list of rules applied to this popup.")
  (put 'doom-popup-rule 'permanent-local t)

  (defvar doom-popup-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map [remap doom/kill-real-buffer] 'doom/popup-close)
      (define-key map [remap evil-window-delete]    'doom/popup-close)
      (define-key map [remap evil-window-move-very-bottom] 'ignore)
      (define-key map [remap evil-window-move-very-top]    'ignore)
      (define-key map [remap evil-window-move-far-left]    'ignore)
      (define-key map [remap evil-window-move-far-right]   'ignore)
      (define-key map [remap evil-window-split]  'ignore)
      (define-key map [remap evil-window-vsplit] 'ignore)
      (define-key map [remap evil-force-normal-state] 'doom/popup-close-maybe)
      (define-key map [escape] 'doom/popup-close-maybe)
      (define-key map (kbd "ESC") 'doom/popup-close-maybe)
      map)
    "Active keymap in popup windows.")

  (define-minor-mode doom-popup-mode
    "Minor mode for pop-up windows. Enables local keymaps and sets state
variables."
    :global nil
    :init-value nil
    :keymap doom-popup-mode-map
    (let ((rules (--any (let ((key (car it)))
                          (when (cond ((symbolp key)
                                       (or (eq major-mode key)
                                           (derived-mode-p key)))
                                      ((stringp key)
                                       (string-match-p key (buffer-name))))
                            (cdr it)))
                        doom-popup-rules)))
      (set-window-dedicated-p nil doom-popup-mode)
      (setq doom-last-popup (current-buffer))
      (setq-local doom-popup-rule rules)))
  (put 'doom-popup-mode 'permanent-local t)

  (defun doom*popup-init (orig-fn &rest args)
    "Enables `doom-popup-mode' in every popup window and returns the window."
    (let ((window (apply orig-fn args)))
      (with-selected-window window
        (doom-popup-mode +1))
      ;; NOTE orig-fn returns a window, so `doom*popup-init' must too
      window))

  (defun doom*popup-save (orig-fun &rest args)
    "Prevents messing up a popup buffer on window changes."
    (let ((in-popup-p (doom-popup-p))
          (popups-p (and doom-last-popup
                         (window-live-p (get-buffer-window doom-last-popup)))))
      (when popups-p
        (mapc (lambda (w) (doom/popup-close w t))
              (-filter 'doom-popup-p (window-list))))
      (unwind-protect (apply orig-fun args)
        (when popups-p
          (let ((origin-win (selected-window)))
            (doom/popup-last-buffer)
            (when in-popup-p
              (select-window origin-win)))))))

  ;; There is no shackle-popup hook, so I created one:
  (advice-add 'shackle-display-buffer :around 'doom*popup-init)
  ;; Order matters for these two
  (advice-add 'balance-windows :around 'doom*popup-save))


;;
;; Hacks
;;

(after! evil
  ;; Tell these functions not to mess with popups:
  (advice-add 'doom-evil-window-move  :around 'doom*popup-save)
  (advice-add 'evil-window-move-very-bottom :around 'doom*popup-save)
  (advice-add 'evil-window-move-very-top    :around 'doom*popup-save)
  (advice-add 'evil-window-move-far-left    :around 'doom*popup-save)
  (advice-add 'evil-window-move-far-right   :around 'doom*popup-save)

  (defun doom*evil-command-window (hist cmd-key execute-fn)
    "The evil command window has a mind of its own (uses `switch-to-buffer'). We
monkey patch it to use pop-to-buffer."
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

  (advice-add 'evil-command-window :override 'doom*evil-command-window))

(after! help-mode
  ;; Help buffers use itself (or `other-window') to decide where to open
  ;; followed links, which can be unpredictable. It should *only* replace the
  ;; original buffer we opened the popup from. To fix this these three button
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
                             (recenter nil))
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
                             (recenter nil))
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
                             (recenter nil))
                         (message "Unable to find location in file"))))))

(after! magit
  ;; Don't open files (from magit) in the magit popup
  (advice-add 'magit-display-file-buffer-traditional :around 'doom*popup-save))

(after! neotree
  (defun doom*save-neotree (orig-fun &rest args)
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
  (advice-add 'shackle-display-buffer :around 'doom*save-neotree)
  ;; Prevents messing up the neotree buffer on window changes
  (advice-add 'doom-evil-window-move :around 'doom*save-neotree)
  ;; (advice-add 'doom-popup-buffer     :around 'doom*save-neotree)
  ;; Don't let neotree interfere with moving, splitting or rebalancing windows
  (advice-add 'balance-windows :around 'doom*save-neotree)
  (advice-add 'split-window    :around 'doom*save-neotree)
  (advice-add 'shackle-display-buffer :around 'doom*save-neotree)
  (advice-add 'evil-window-move-very-bottom :around 'doom*save-neotree)
  (advice-add 'evil-window-move-very-top    :around 'doom*save-neotree)
  (advice-add 'evil-window-move-far-left    :around 'doom*save-neotree)
  (advice-add 'evil-window-move-far-right   :around 'doom*save-neotree))

(add-hook! org-load
  ;; Ensures org-src-edit yields control of its buffer to shackle.
  (defun org-src-switch-to-buffer (buffer context)
    (pop-to-buffer buffer))

  ;; And these for org-todo, org-link and org-agenda
  (defun org-pop-to-buffer-same-window (&optional buffer-or-name norecord label)
    "Pop to buffer specified by BUFFER-OR-NAME in the selected window."
    (display-buffer buffer-or-name))

  (defun org-switch-to-buffer-other-window (&rest args)
    (car-safe
     (mapc (lambda (b)
             (let ((buf (if (stringp b) (get-buffer-create b) b)))
               (pop-to-buffer buf t t)))
           args)))

  (defun doom/org-agenda-quit ()
    "Necessary to finagle org-agenda into shackle popups and behave properly on quit."
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
    (map! :map org-agenda-mode-map
          :e "<escape>" 'doom/org-agenda-quit
          :e "ESC" 'doom/org-agenda-quit
          :e [escape] 'doom/org-agenda-quit
          "q" 'doom/org-agenda-quit
          "Q" 'doom/org-agenda-quit)))


;;
;; Functions
;;

(defun doom-popup-p (&optional window)
  "Whether WINDOW is a popup window or not. If WINDOW is nil, use current
window. Returns nil or the popup window."
  (setq window (or window (selected-window)))
  (and (window-live-p window)
       (buffer-local-value 'doom-popup-mode (window-buffer window))
       window))

(defun doom-popup-buffer (buffer &optional plist)
  "Display BUFFER in a shackle popup."
  (let* ((buffer-name (cond ((stringp buffer) buffer)
                            ((bufferp buffer) (buffer-name buffer))
                            (t (error "Not a valid buffer"))))
         (buffer (get-buffer-create buffer-name)))
    (when (doom/real-buffer-p (window-buffer))
      (setq doom-last-window (selected-window)))
    (shackle-display-buffer
     buffer
     nil (or plist (shackle-match buffer-name)))))

(defun doom/popup-messages ()
  "Pop up the messages buffer."
  (interactive)
  (doom-popup-buffer (messages-buffer))
  (goto-char (point-max)))

(defun doom/popup-last-buffer ()
  "Restore the last popup."
  (interactive)
  (unless (buffer-live-p doom-last-popup)
    (setq doom-last-popup nil)
    (error "No popup to restore"))
  (doom-popup-buffer doom-last-popup))

(defun doom/popup-close (&optional window dont-kill)
  "Find and close the currently active popup (if available)."
  (interactive)
  (setq window (or window (selected-window)))
  (when (doom-popup-p window)
    (with-selected-window window
      ;; If REPL...
      (when (bound-and-true-p repl-toggle-mode)
        (setq rtog/--last-buffer nil))
      (doom-popup-mode -1)
      (unless (or dont-kill (memq :nokill doom-popup-rule))
        (let ((kill-buffer-query-functions
               (delq 'process-kill-buffer-query-function
                     kill-buffer-query-functions)))
          (kill-buffer (window-buffer window)))))
    (delete-window window)))

(defun doom/popup-close-maybe ()
  "Close the current popup *if* its buffer doesn't have a :noesc rule in
`doom-popup-rules'."
  (interactive)
  (if (memq :noesc doom-popup-rule)
      (call-interactively 'evil-force-normal-state)
    (doom/popup-close)))

(defun doom/popup-close-all (&optional dont-kill)
  "Closes all popups (kill them if DONT-KILL-BUFFERS is non-nil)."
  (interactive)
  (let ((orig-win (selected-window)))
    (mapc (lambda (w) (doom/popup-close w dont-kill))
          (--filter (and (doom-popup-p it) (not (eq it orig-win)))
                    (window-list)))))

(provide 'core-popups)
;;; core-popups.el ends here
