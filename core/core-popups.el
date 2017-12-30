;;; core-popups.el -*- lexical-binding: t; -*-

;; I want a "real"-buffer-first policy in my Emacsian utpoia; popup buffers
;; ought to be second-class citizens to "real" buffers. No need for a wall or
;; controversial immigration policies -- all we need is `shackle' (and it will
;; actually work).
;;
;; The gist is: popups should be displayed on one side of the frame, away from
;; 'real' buffers. They should be easy to dispose of when we don't want to see
;; them and easily brought back in case we change our minds. Also, popups should
;; typically have no mode-line.
;;
;; Be warned, this requires a lot of hackery voodoo that could break with an
;; emacs update or an update to any of the packages it tries to tame (like helm
;; or org-mode).

(defvar doom-popup-history nil
  "A list of popups that were last closed. Used by `doom/popup-restore' and
`doom*popups-save'.")

(defvar doom-popup-other-window nil
  "The last window selected before a popup was opened.")

(defvar doom-popup-no-fringes t
  "If non-nil, disable fringes in popup windows.")

(defvar doom-popup-windows ()
  "A list of open popup windows.")

(defvar-local doom-popup-rules nil
  "The shackle rule that caused this buffer to be recognized as a popup. Don't
edit this directly.")
(put 'doom-popup-rules 'permanent-local t)

(defvar doom-popup-window-parameters
  '(:noesc :modeline :autokill :autoclose :autofit :static)
  "A list of window parameters that are set (and cleared) when `doom-popup-mode
is enabled/disabled.'")

(defvar doom-popup-remember-history t
  "Don't modify this directly. If non-nil, DOOM will remember the last popup(s)
that was/were open in `doom-popup-history'.")

(defvar doom-popup-inhibit-autokill nil
  "Don't modify this directly. When it is non-nil, no buffers will be killed
when their associated popup windows are closed, despite their :autokill
property.")

(defvar doom-popup-mode-map (make-sparse-keymap)
  "Active keymap in popup windows.")


(def-setting! :popup (&rest rules)
  "Prepend a new popup rule to `shackle-rules' (see for format details).

Several custom properties have been added that are not part of shackle, but are
recognized by DOOM's popup system. They are:

:noesc      If non-nil, the popup won't be closed if you press ESC from *inside*
            its window. Used by `doom/popup-close-maybe'.

:modeline   By default, mode-lines are hidden in popups unless this is non-nil.
            If it is a symbol, it'll use `doom-modeline' to fetch a modeline
            config (in `doom-popup-mode').

:autokill   If non-nil, the popup's buffer will be killed when the popup is
            closed. Used by `doom*delete-popup-window'. NOTE
            `doom/popup-restore' can't restore non-file popups that have an
            :autokill property.

:autoclose  If non-nil, close popup if ESC is pressed from outside the popup
            window.

:autofit    If non-nil, resize the popup to fit its content. Uses the value of
            the :size property as the maximum height/width. This will not work
            if the popup has no content when displayed.

:static     If non-nil, don't treat this window like a popup. This makes it
            impervious to being automatically closed or tracked in popup
            history. Excellent for permanent sidebars."
  (if (cl-every #'listp (mapcar #'doom-unquote rules))
      `(setq shackle-rules (nconc (list ,@rules) shackle-rules))
    `(push (list ,@rules) shackle-rules)))


;;
;;
;;

;; (defvar doom-popup-parameters
;;   '(:esc :modeline :transient :fit :align :size)
;;   "TODO")

;; (defvar doom-popup-whitelist
;;   '(("^ ?\\*" :size 15 :noselect t :autokill t :autoclose t))
;;   "TODO")

(defvar doom-popup-blacklist
  '("^\\*magit")
  "TODO")


;;
;; Bootstrap
;;

(def-package! shackle
  :init
  (setq shackle-default-alignment 'below
        shackle-default-size 8
        shackle-rules
        '(("^\\*eww" :regexp t :size 0.5 :select t :autokill t :noesc t)
          ("^\\*ftp " :noselect t :autokill t :noesc t)
          ;; doom
          ("^\\*doom:scratch" :regexp t :size 15 :noesc t :select t :modeline t :autokill t :static t)
          ("^\\*doom:" :regexp t :size 0.35 :noesc t :select t)
          ("^ ?\\*doom " :regexp t :noselect t :autokill t :autoclose t :autofit t)
          ;; built-in (emacs)
          ("*compilation*" :size 0.25 :noselect t :autokill t :autoclose t)
          ("*ert*" :same t :modeline t)
          ("*info*" :size 0.5 :select t :autokill t)
          ("*Backtrace*" :size 20 :noselect t)
          ("*Warnings*"  :size 12 :noselect t :autofit t)
          ("*Messages*"  :size 12 :noselect t)
          ("*Help*" :size 0.3 :autokill t)
          ("^\\*.*Shell Command.*\\*$" :regexp t :size 20 :noselect t :autokill t)
          (apropos-mode :size 0.3 :autokill t :autoclose t)
          (Buffer-menu-mode :size 20 :autokill t)
          (comint-mode :noesc t)
          (grep-mode :size 25 :noselect t :autokill t)
          (profiler-report-mode :size 0.3 :regexp t :autokill t :modeline minimal)
          (tabulated-list-mode :noesc t)
          ("^ ?\\*" :regexp t :size 15 :noselect t :autokill t :autoclose t)))

  :config
  ;; NOTE This is a temporary fix while I rewrite core-popups
  (defun doom-display-buffer-condition (buffer _action)
    (and (cl-loop for re in doom-popup-blacklist
                  when (string-match-p re buffer)
                  return nil
                  finally return t)
         (shackle-match buffer)))

  (defun doom-display-buffer-action (buffer alist)
    (shackle-display-buffer buffer alist (shackle-match buffer)))

  (defun doom|autokill-popups ()
    (or (not (doom-popup-p))
        (prog1 (when (and (not doom-popup-inhibit-autokill)
                          (plist-get doom-popup-rules :autokill))
                 (doom-popup-mode -1)
                 (when-let* ((process (get-buffer-process (current-buffer))))
                   (set-process-query-on-exit-flag process nil))
                 t))))

  (add-hook! doom-post-init
    (setq display-buffer-alist
          (cons '(doom-display-buffer-condition doom-display-buffer-action)
                display-buffer-alist))
    (add-hook 'kill-buffer-query-functions #'doom|autokill-popups))

  ;; no modeline in popups
  (add-hook 'doom-popup-mode-hook #'doom|hide-modeline-in-popup)
  ;; ensure every rule without an :align, :same or :frame property has an
  ;; implicit :align (see `shackle-default-alignment')
  (advice-add #'shackle--match :filter-return #'doom*shackle-always-align)

  ;; bootstrap popup system
  (advice-add #'shackle-display-buffer :around #'doom*popup-init)
  (advice-add #'balance-windows :around #'doom*popups-save)
  (advice-add #'delete-window :before #'doom*delete-popup-window)

  ;; Tell `window-state-get' and `current-window-configuration' to recognize
  ;; these custom parameters. Helpful for `persp-mode' and persisting window
  ;; configs that have popups in them.
  (dolist (param `(popup ,@doom-popup-window-parameters))
    (push (cons param 'writable) window-persistent-parameters))

  (let ((map doom-popup-mode-map))
    (define-key map [escape]    #'doom/popup-close-maybe)
    (define-key map (kbd "ESC") #'doom/popup-close-maybe)
    (define-key map [remap quit-window] #'doom/popup-close-maybe)
    (define-key map [remap doom/kill-this-buffer] #'doom/popup-close-maybe)
    (define-key map [remap split-window-right]              #'ignore)
    (define-key map [remap split-window-below]              #'ignore)
    (define-key map [remap split-window-horizontally]       #'ignore)
    (define-key map [remap split-window-vertically]         #'ignore)
    (define-key map [remap mouse-split-window-horizontally] #'ignore)
    (define-key map [remap mouse-split-window-vertically]   #'ignore)))


;;
;; Hacks
;;

(progn ; hacks for built-in functions
  (defun doom*suppress-pop-to-buffer-same-window (orig-fn &rest args)
    (cl-letf (((symbol-function 'pop-to-buffer-same-window)
               (symbol-function 'pop-to-buffer)))
      (apply orig-fn args)))
  (advice-add #'info :around #'doom*suppress-pop-to-buffer-same-window)
  (advice-add #'eww :around #'doom*suppress-pop-to-buffer-same-window)
  (advice-add #'eww-browse-url :around #'doom*suppress-pop-to-buffer-same-window)

  (defun doom*popup-buffer-menu (&optional arg)
    "Open `buffer-menu' in a popup window."
    (interactive "P")
    (with-selected-window (doom-popup-buffer (list-buffers-noselect arg))
      (setq mode-line-format "Commands: d, s, x, u; f, o, 1, 2, m, v; ~, %; q to quit; ? for help.")))
  (advice-add #'buffer-menu :override #'doom*popup-buffer-menu))


(after! comint
  (defun doom|popup-close-comint-buffer ()
    (when (and (doom-popup-p)
               (derived-mode-p 'comint-mode)
               (not (process-live-p (get-buffer-process (current-buffer)))))
      (delete-window)))
  (add-hook '+evil-esc-hook #'doom|popup-close-comint-buffer t))


(after! eshell
  ;; By tying buffer life to its process, we ensure that we land back in the
  ;; eshell buffer after term dies. May cause problems with short-lived
  ;; processes.
  ;; FIXME replace with a 'kill buffer' keybinding.
  (setq eshell-destroy-buffer-when-process-dies t)

  ;; When eshell runs a visual command (see `eshell-visual-commands'), it spawns
  ;; a term buffer to run it in, but where it spawns it is the problem...
  (defun doom*eshell-undedicate-popup (orig-fn &rest args)
    "Force spawned term buffer to share with the eshell popup (if necessary)."
    (when (doom-popup-p)
      (set-window-dedicated-p nil nil)
      (add-transient-hook! #'eshell-query-kill-processes :after
        (set-window-dedicated-p nil t)))
    (apply orig-fn args))
  (advice-add #'eshell-exec-visual :around #'doom*eshell-undedicate-popup))


(after! evil
  (let ((map doom-popup-mode-map))
    (define-key map [remap evil-window-delete]           #'doom/popup-close-maybe)
    (define-key map [remap evil-save-modified-and-close] #'doom/popup-close-maybe)
    (define-key map [remap evil-window-move-very-bottom] #'doom/popup-move-bottom)
    (define-key map [remap evil-window-move-very-top]    #'doom/popup-move-top)
    (define-key map [remap evil-window-move-far-left]    #'doom/popup-move-left)
    (define-key map [remap evil-window-move-far-right]   #'doom/popup-move-right)
    (define-key map [remap evil-window-split]            #'ignore)
    (define-key map [remap evil-window-vsplit]           #'ignore))

  (defun doom|popup-close-maybe ()
    "If current window is a popup, close it. If minibuffer is open, close it. If
not in a popup, close all popups with an :autoclose property."
    (if (doom-popup-p)
        (unless (doom-popup-property :noesc)
          (delete-window))
      (doom/popup-close-all)))
  (add-hook '+evil-esc-hook #'doom|popup-close-maybe t)

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


(after! helm
  ;; Helm tries to clean up after itself, but shackle has already done this,
  ;; causing problems. This fixes that. To reproduce, add a helm rule in
  ;; `shackle-rules', open two splits side-by-side, move to the buffer on the
  ;; right and invoke helm. It will close all but the left-most buffer.
  (setq-default helm-reuse-last-window-split-state t
                helm-split-window-in-side-p t)

  (after! helm-swoop
    (setq helm-swoop-split-window-function #'pop-to-buffer))

  (after! helm-ag
    ;; This prevents helm-ag from switching between windows and buffers.
    (defun doom*helm-ag-edit-done (orig-fn &rest args)
      (cl-letf (((symbol-function 'select-window) #'ignore))
        (apply orig-fn args))
      (doom/popup-close))
    (advice-add #'helm-ag--edit-commit :around #'doom*helm-ag-edit-done)
    (advice-add #'helm-ag--edit-abort  :around #'doom*helm-ag-edit-done)

    (defun doom*helm-ag-edit (orig-fn &rest args)
      (cl-letf (((symbol-function 'other-window) #'ignore)
                ((symbol-function 'switch-to-buffer) #'doom-popup-buffer))
        (apply orig-fn args)
        (with-current-buffer (get-buffer "*helm-ag-edit*")
          (use-local-map helm-ag-edit-map))))
    (advice-add #'helm-ag--edit :around #'doom*helm-ag-edit)))


(defsubst doom--switch-from-popup (location)
  (doom/popup-close)
  (switch-to-buffer (car location) nil t)
  (if (not (cdr location))
      (message "Unable to find location in file")
    (goto-char (cdr location))
    (recenter)))

(after! help-mode
  ;; Help buffers use `other-window' to decide where to open followed links,
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


(after! magit
  (add-hook 'magit-mode-hook #'doom-hide-modeline-mode))


(after! mu4e
  (defun doom*mu4e-popup-window (buf _height)
    (doom-popup-buffer buf '(:size 10 :noselect t))
    buf)
  (advice-add #'mu4e~temp-window :override #'doom*mu4e-popup-window))


(after! multi-term
  (setq multi-term-buffer-name "doom:terminal"))


(after! neotree
  ;; Neotree has its own window/popup management built-in, which is difficult to
  ;; police. For example, switching perspectives will cause neotree to forget it
  ;; is a neotree pane.
  ;;
  ;; By handing neotree over to shackle, which is better integrated into the
  ;; rest of my config (and persp-mode), this is no longer a problem.
  (set! :popup " *NeoTree*" :align neo-window-position :size neo-window-width :static t)

  (defun +evil-neotree-display-fn (buf _alist)
    "Hand neotree off to shackle."
    (let ((win (doom-popup-buffer buf)))
      (setq neo-global--buffer (window-buffer win)
            neo-global--window win)))
  (setq neo-display-action '(+evil-neotree-display-fn))

  (defun +evil|neotree-fix-popup ()
    "Repair neotree state whenever its popup state is restored. This ensures
that `doom*popup-save' won't break it."
    (when (equal (buffer-name) neo-buffer-name)
      (setq neo-global--window (selected-window))
      ;; Fix neotree shrinking when closing nearby vertical splits
      (when neo-window-fixed-size
        (doom-resize-window neo-global--window neo-window-width t t))))
  (add-hook 'doom-popup-mode-hook #'+evil|neotree-fix-popup))


(after! persp-mode
  (defun doom*persp-mode-restore-popups (&rest _)
    "Restore popup windows when loading a perspective from file."
    (dolist (window (window-list))
      (when-let* ((plist (doom-popup-properties window)))
        (with-selected-window window
          (unless doom-popup-mode
            (setq-local doom-popup-rules plist)
            (doom-popup-mode +1))))))
  (advice-add #'persp-load-state-from-file :after #'doom*persp-mode-restore-popups))


(after! quickrun
  ;; don't auto-focus quickrun windows, shackle handles that
  (setq quickrun-focus-p nil))


(after! twittering-mode
  (setq twittering-pop-to-buffer-function #'pop-to-buffer))


(after! wgrep
  ;; close the popup after you're done with a wgrep buffer
  (advice-add #'wgrep-abort-changes :after #'doom/popup-close)
  (advice-add #'wgrep-finish-edit   :after #'doom/popup-close))


(after! xref
  (defun doom*xref-follow-and-close (orig-fn &rest args)
    "Jump to the xref on the current line, select its window and close the popup
you came from."
    (interactive)
    (let ((popup-p (doom-popup-p))
          (window (selected-window)))
      (apply orig-fn args)
      (when popup-p (doom/popup-close window))))
  (advice-add #'xref-goto-xref :around #'doom*xref-follow-and-close))


;;
;; Major modes
;;

(after! plantuml-mode
  (defun doom*plantuml-preview-in-popup-window (orig-fn &rest args)
    (save-window-excursion
      (apply orig-fn args))
    (pop-to-buffer plantuml-preview-buffer))
  (advice-add #'plantuml-preview-string
              :around #'doom*plantuml-preview-in-popup-window))

;; Ensure these settings are loaded as late as possible, giving other modules a
;; chance to reconfigure org popup settings before the defaults kick in.
(defun doom|init-org-popups ()
  (add-hook! org-load
    (set! :popup
      '("*Calendar*"         :size 0.4 :noselect t)
      '(" *Org todo*"        :size 5   :noselect t)
      '("*Org Note*"         :size 10)
      '("*Org Select*"       :size 20  :noselect t)
      '("*Org Links*"        :size 5   :noselect t)
      '("*Org Export Dispatcher*" :noselect t)
      '(" *Agenda Commands*" :noselect t)
      '("^\\*Org Agenda"     :regexp t :size 20)
      '("*Org Clock*"        :noselect t)
      '("^\\*Org Src"        :regexp t :size 0.35 :noesc t)
      '("*Edit Formulas*"    :size 10)
      '("^\\*Org-Babel"      :regexp t :size 25 :noselect t)
      '("^CAPTURE.*\\.org$"  :regexp t :size 20))

    ;; Org has a scorched-earth window management system I'm not fond of. i.e.
    ;; it kills all windows and monopolizes the frame. No thanks. We can do
    ;; better with shackle's help.
    (defun doom*suppress-delete-other-windows (orig-fn &rest args)
      (cl-letf (((symbol-function 'delete-other-windows)
                 (symbol-function 'ignore)))
        (apply orig-fn args)))
    (advice-add #'org-add-log-note :around #'doom*suppress-delete-other-windows)
    (advice-add #'org-capture-place-template :around #'doom*suppress-delete-other-windows)
    (advice-add #'org-export--dispatch-ui :around #'doom*suppress-delete-other-windows)

    ;; Hand off the src-block window to a shackle popup window.
    (defun doom*org-src-pop-to-buffer (buffer _context)
      "Open the src-edit in a way that shackle can detect."
      (if (eq org-src-window-setup 'switch-invisibly)
          (set-buffer buffer)
        (pop-to-buffer buffer)))
    (advice-add #'org-src-switch-to-buffer :override #'doom*org-src-pop-to-buffer)

    ;; Ensure todo, agenda, and other minor popups are delegated to shackle.
    (defun doom*org-pop-to-buffer (&rest args)
      "Use `pop-to-buffer' instead of `switch-to-buffer' to open buffer.'"
      (let ((buf (car args)))
        (pop-to-buffer
         (cond ((stringp buf) (get-buffer-create buf))
               ((bufferp buf) buf)
               (t (error "Invalid buffer %s" buf))))))
    (advice-add #'org-switch-to-buffer-other-window :override #'doom*org-pop-to-buffer)

    ;; org-agenda
    (setq org-agenda-window-setup 'other-window
          org-agenda-restore-windows-after-quit nil)
      ;; Hide modeline in org-agenda
    (add-hook 'org-agenda-finalize-hook #'doom-hide-modeline-mode)
    (add-hook 'org-agenda-finalize-hook #'org-fit-window-to-buffer)
    ;; Don't monopolize frame!
    (advice-add #'org-agenda :around #'doom*suppress-delete-other-windows)
    ;; ensure quit keybindings work propertly
    (map! :map* org-agenda-mode-map
          :m [escape] 'org-agenda-Quit
          :m "ESC"    'org-agenda-Quit)))
(add-hook 'doom-init-hook #'doom|init-org-popups)

(provide 'core-popups)
;;; core-popups.el ends here
