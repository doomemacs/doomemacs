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

(defvar doom-popup-window-parameters
  '(:noesc :modeline :autokill :autoclose)
  "A list of window parameters that are set (and cleared) when `doom-popup-mode
is enabled/disabled.'")

(def-setting! :popup (&rest rules)
  "Prepend a new popup rule to `shackle-rules'."
  (if (cl-every 'listp rules)
      `(setq shackle-rules (nconc ',rules shackle-rules))
    `(push ',rules shackle-rules)))


;;
;; Bootstrap
;;

(def-package! shackle :demand t
  :init
  (setq shackle-default-alignment 'below
        ;;; Baseline popup-window rules
        ;; Several custom properties have been added that are not part of
        ;; shackle and are used by doom's popup system. They are:
        ;;
        ;;  :noesc      Determines if pressing ESC *inside* the popup should
        ;;              close it. Used by `doom/popup-close-maybe'.
        ;;  :modeline   By default, mode-lines are hidden in popups unless this
        ;;              is non-nil. If it is a symbol, it'll use `doom-modeline'
        ;;              to fetch a modeline config. Set in `doom-popup-mode'.
        ;;  :autokill   If non-nil, the buffer in these popups will be killed
        ;;              when their popup is closed. Used by
        ;;              `doom*delete-popup-window'. NOTE This will render
        ;;              `doom/popup-restore' unusable for this buffer.
        ;;  :autoclose  If non-nil, close popup if ESC is pressed from any buffer.
        shackle-rules
        '(("^ ?\\*doom:.+\\*$"      :size 25  :modeline minimal :regexp t :noesc t)
          ("^ ?\\*doom .+\\*$"      :size 10  :noselect t :regexp t)
          ("^ *doom message*"       :size 10  :noselect t :autokill t)
          ("*Metahelp*"             :size 0.5 :autokill t :autoclose t)
          ("^\\*.+-Profiler-Report .+\\*$" :size 0.3 :regexp t :autokill t)
          ("*minor-modes*"          :size 0.5 :noselect t :autokill t)
          ("*eval*"                 :size 16  :noselect t :autokill t :autoclose t)
          ("*Pp Eval Output*"       :size 16  :noselect t :autokill t :autoclose t)
          ("*Apropos*"              :size 0.3)
          ("*Backtrace*"            :size 25  :noselect t)
          ("*Buffer List*"          :size 20  :autokill t)
          ("*Help*"                 :size 16)
          ("*Messages*"             :size 10  :noselect t)
          ("*Warnings*"             :size 10  :noselect t :autokill t)
          ("*command-log*"          :size 28  :noselect t :align right)
          ("*Shell Command Output*" :size 20  :noselect t :autokill t)
          ("*Occur*"                :size 25  :noselect t :autokill t)
          ("*Error*"                :size 10  :noselect t :autokill t :autoclose t)
          ("*Process List*"         :size 10  :noselect t :autokill t :autoclose t)
          ("*Keys*"                 :size 10  :noselect t)
          ("^\\*ftp "               :size 8   :noselect t :autokill t :noesc t)
          (compilation-mode         :size 15  :noselect t)
          (eww-mode                 :size 30)
          (comint-mode              :noesc t)
          (tabulated-list-mode      :noesc t)))

  :config
  (if (display-graphic-p)
      (shackle-mode +1)
    (add-transient-hook! 'after-make-frame-functions (shackle-mode +1))
    (add-hook 'after-init-hook 'shackle-mode))

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
  (advice-add #'shackle--match :filter-return #'doom*shackle-always-align))


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
    (define-key map [remap doom-kill-buffer] 'kill-this-buffer)
    (define-key map [remap doom/kill-this-buffer] 'kill-this-buffer)
    (define-key map [remap split-window-right] 'ignore)
    (define-key map [remap split-window-below] 'ignore)
    (define-key map [remap split-window-horizontally] 'ignore)
    (define-key map [remap split-window-vertically] 'ignore)
    (define-key map [remap mouse-split-window-horizontally] 'ignore)
    (define-key map [remap mouse-split-window-vertically] 'ignore)
    map)
  "Active keymap in popup windows.")

(define-minor-mode doom-popup-mode
  "Minor mode for popup windows."
  :init-value nil
  :keymap doom-popup-mode-map
  (let ((window (selected-window)))
    ;; Ensure that buffer-opening functions/commands (like
    ;; `switch-to-buffer-other-window' won't use this window).
    (set-window-parameter window 'no-other-window doom-popup-mode)
    ;; Makes popup window resist interactively changing its buffer.
    (set-window-dedicated-p window doom-popup-mode)
    (cond (doom-popup-mode
           ;; Save metadata into window parameters so it can be saved by window
           ;; config persisting plugins like workgroups or persp-mode.
           (set-window-parameter window 'popup (or doom-popup-rules t))
           (when doom-popup-rules
             (dolist (param doom-popup-window-parameters)
               (when-let (val (plist-get doom-popup-rules param))
                 (set-window-parameter window param val)))))

          (t
           ;; Ensure window parameters are cleaned up
           (set-window-parameter window 'popup nil)
           (dolist (param doom-popup-window-parameters)
             (set-window-parameter window param nil))))))

;; Major mode changes (and other things) may call `kill-all-local-variables',
;; turning off things like `doom-popup-mode'. This prevents that.
(put 'doom-popup-mode 'permanent-local t)
(put 'doom-popup-rules 'permanent-local t)

;; Don't show modeline in popup windows without a :modeline rule. If
;; one exists and it's a symbol, use `doom-modeline' to grab the
;; format. If non-nil, show the mode-line as normal. If nil (or
;; omitted, by default), then hide the modeline entirely.
(add-hook! 'doom-popup-mode-hook
  (if doom-popup-mode
      (let ((modeline (plist-get doom-popup-rules :modeline)))
        (cond ((or (eq modeline 'nil)
                   (not modeline))
               (doom-hide-modeline-mode +1))
              ((and (symbolp modeline)
                    (not (eq modeline 't)))
               (setq-local doom--modeline-format (doom-modeline modeline))
               (when doom--modeline-format
                 (doom-hide-modeline-mode +1)))))
    ;; show modeline
    (when doom-hide-modeline-mode
      (doom-hide-modeline-mode -1))))

;;
(defun doom*popup-init (orig-fn &rest args)
  "Initializes a window as a popup window by enabling `doom-popup-mode' in it
and setting `doom-popup-rules' within it. Returns the window."
  (unless (doom-popup-p)
    (setq doom-popup-other-window (selected-window)))
  (let ((plist (or (nth 2 args)
                   (cond ((windowp (car args))
                          (shackle-match (window-buffer (car args))))
                         ((bufferp (car args))
                          (shackle-match (car args))))))
        (buffer (get-buffer (car args)))
        window)
    (when (and (doom-real-buffer-p buffer)
               (get-buffer-window-list buffer nil t))
      (setq plist (append (list :autokill t) plist))
      (setcar args (clone-indirect-buffer (buffer-name (car args)) nil t)))
    (unless (setq window (apply orig-fn args))
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
      (mapc #'doom/popup-close popups))
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
      (let ((autokill-p (plist-get doom-popup-rules :autokill)))
        (with-selected-window window
          (doom-popup-mode -1)
          (when autokill-p
            (kill-buffer (current-buffer))))))))

(advice-add #'shackle-display-buffer :around #'doom*popup-init)
(advice-add #'balance-windows :around #'doom*popups-save)
(advice-add #'delete-window :before #'doom*delete-popup-window)


;;
;; Hacks
;;

(progn ; hacks for built-in functions
  (defun doom*buffer-menu (&optional arg)
    "Open `buffer-menu' in a popup window."
    (interactive "P")
    (let ((buf (list-buffers-noselect arg)))
      (doom-popup-buffer buf)
      (with-current-buffer buf
        (setq mode-line-format "Commands: d, s, x, u; f, o, 1, 2, m, v; ~, %; q to quit; ? for help."))))
  (advice-add #'buffer-menu :override #'doom*buffer-menu))


(after! eshell
  ;; When eshell runs a visual command (see `eshell-visual-commands'), it spawns
  ;; a term buffer to run it in, but where it spawns it is the problem.

  ;; By tying buffer life to its process, we ensure that we land back in the
  ;; eshell buffer after term dies. May cause problems with short-lived
  ;; processes.
  ;; FIXME replace with a 'kill buffer' keybinding.
  (setq eshell-destroy-buffer-when-process-dies t)

  (defun doom*eshell-undedicate-popup (orig-fn &rest args)
    "Force spawned term buffer to share with the eshell popup (if necessary)."
    (when (doom-popup-p)
      (set-window-dedicated-p nil nil)
      (add-transient-hook! eshell-query-kill-processes :after
        (set-window-dedicated-p nil t)))
    (apply orig-fn args))
  (advice-add #'eshell-exec-visual :around #'doom*eshell-undedicate-popup))


(after! evil
  (let ((map doom-popup-mode-map))
    (define-key map [remap evil-window-delete]           'doom/popup-close)
    (define-key map [remap evil-save-modified-and-close] 'doom/popup-close)
    (define-key map [remap evil-window-move-very-bottom] 'ignore)
    (define-key map [remap evil-window-move-very-top]    'ignore)
    (define-key map [remap evil-window-move-far-left]    'ignore)
    (define-key map [remap evil-window-move-far-right]   'ignore)
    (define-key map [remap evil-window-split]            'ignore)
    (define-key map [remap evil-window-vsplit]           'ignore)
    (define-key map [remap evil-force-normal-state]      'doom/popup-close-maybe))

  (defun doom|popup-close-all-maybe ()
    "Close popups with an :autoclose property when pressing ESC from normal
mode in any evil-mode buffer."
    (unless (or (doom-popup-p)
                (minibuffer-window-active-p (minibuffer-window))
                (and (bound-and-true-p evil-mode)
                     (evil-ex-hl-active-p 'evil-ex-search)))
      (doom/popup-close-all)))
  (add-hook '+evil-esc-hook #'doom|popup-close-all-maybe)

  ;; Make evil-mode cooperate with popups
  (advice-add #'evil-command-window :override #'doom*popup-evil-command-window)
  (advice-add #'evil-command-window-execute :override #'doom*popup-evil-command-window-execute)

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
  (advice-add #'doom-evil-window-move        :around #'doom*popups-save)
  (advice-add #'evil-window-move-very-bottom :around #'doom*popups-save)
  (advice-add #'evil-window-move-very-top    :around #'doom*popups-save)
  (advice-add #'evil-window-move-far-left    :around #'doom*popups-save)
  (advice-add #'evil-window-move-far-right   :around #'doom*popups-save)

  ;; Don't block moving to/from popup windows
  (defun doom*ignore-window-parameters-in-popups (dir &optional arg window)
    (window-in-direction (cond ((eq dir 'up)   'above)
                               ((eq dir 'down) 'below)
                               (t dir))
                         window t arg windmove-wrap-around t))
  (advice-add #'windmove-find-other-window :override #'doom*ignore-window-parameters-in-popups))


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


(after! mu4e
  (defun doom*mu4e-popup-window (buf height)
    (doom-popup-buffer buf :size 10 :noselect t)
    buf)
  (advice-add #'mu4e~temp-window :override #'doom*mu4e-popup-window))


(after! neotree
  ;; Neotree has its own window/popup management built-in, which is difficult to
  ;; police. For example, switching perspectives will cause neotree to forget it
  ;; is a neotree pane.
  ;;
  ;; By handing neotree over to shackle, which is better integrated into the
  ;; rest of my config (and persp-mode), this is no longer a problem.
  (setq neo-display-action '(+evil-neotree-display-fn))
  (set! :popup " *NeoTree*" :align 'left :size 25)

  (defun +evil-neotree-display-fn (buf _alist)
    (doom-popup-buffer buf)))


(after! quickrun
  ;; don't auto-focus quickrun windows, shackle handles that
  (setq quickrun-focus-p nil))


(after! twittering-mode
  (setq twittering-pop-to-buffer-function #'pop-to-buffer))


(after! xref
  (defun doom*xref-follow-and-close (orig-fn &rest args)
    "Jump to the xref on the current line, select its window and close the popup
you came from."
    (interactive)
    (let ((popup-p (doom-popup-p))
          (window (selected-window)))
      (apply orig-fn args)
      (when popup-p (doom/popup-close window))))
  (advice-add 'xref-goto-xref :around 'doom*xref-follow-and-close))


;; Ensure these settings are attached to org-load-hook as late as possible,
;; giving other modules to add their own hooks.
(add-hook! 'after-init-hook
  (add-hook! 'org-load-hook
    (set! :popup
      '("*Calendar*"         :size 0.4 :noselect t)
      '(" *Org todo*"        :size 5   :noselect t)
      '("*Org Note*"         :size 10)
      '("*Org Select*"       :size 20  :noselect t)
      '("*Org Links*"        :size 5   :noselect t)
      '("*Org Export Dispatcher*" :noselect t)
      '(" *Agenda Commands*" :noselect t)
      '("^\\*Org Agenda"     :regexp t :size 30)
      '("*Org Clock*"        :noselect t)
      '("^\\*Org Src"        :regexp t :size 0.35 :noesc t)
      '("*Edit Formulas*"    :size 10)
      '("^\\*Org-Babel"      :regexp t :size 25 :noselect t)
      '("^CAPTURE.*\\.org$"  :regexp t :size 20))

    ;; Org has its own window management system with a scorched earth philosophy
    ;; I'm not fond of. i.e. it kills all windows and greedily monopolizes the
    ;; frame. No thanks. We can do better with shackle's help.

    ;; Save the emacsverse from armageddon by suppressing `delete-other-windows'
    ;; in org functions.
    (defun doom*suppress-delete-other-windows (orig-fn &rest args)
      (cl-letf (((symbol-function 'delete-other-windows)
                 (symbol-function 'ignore)))
        (apply orig-fn args)))
    (advice-add #'org-add-log-note :around #'doom*suppress-delete-other-windows)
    (advice-add #'org-capture-place-template :around #'doom*suppress-delete-other-windows)
    (advice-add #'org-export--dispatch-ui :around #'doom*suppress-delete-other-windows)

    ;; `org-edit-src-code' simply clones and narrows the buffer to a src block,
    ;; so we are secretly manipulating the same buffer. Since truely killing it
    ;; would kill the original org buffer we've got to do things differently.
    (defun doom*org-src-switch-to-buffer (buffer context)
      (if (eq org-src-window-setup 'switch-invisibly)
          (set-buffer buffer)
        (pop-to-buffer buffer)))
    (advice-add #'org-src-switch-to-buffer :override #'doom*org-src-switch-to-buffer)

    ;; Ensure todo, agenda, and other minor popups handed off to shackle.
    (defun doom*org-pop-to-buffer (&rest args)
      (let ((buf (car args)))
        (pop-to-buffer
         (cond ((stringp buf) (get-buffer-create buf))
               ((bufferp buf) buf)
               (t (error "Invalid buffer %s" buf))))))
    (advice-add #'org-switch-to-buffer-other-window :override #'doom*org-pop-to-buffer)

    (after! org-agenda
      (setq org-agenda-window-setup 'other-window
            org-agenda-restore-windows-after-quit nil)

      ;; Hide modeline in org-agenda
      (add-hook 'org-agenda-finalize-hook #'doom-hide-modeline-mode)
      ;; Don't monopolize frame!
      (advice-add #'org-agenda :around #'doom*suppress-delete-other-windows)

      (after! evil
        (map! :map* org-agenda-mode-map
              :m [escape] 'org-agenda-Quit
              :m "ESC"    'org-agenda-Quit))
      (let ((map org-agenda-mode-map))
        (define-key map "q" 'org-agenda-Quit)
        (define-key map "Q" 'org-agenda-Quit)))))

(provide 'core-popups)
;;; core-popups.el ends here
