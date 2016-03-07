;;; core-popup.el --- taming stray windows

;; The following is a whole slew of hackery to get Emacs to treat 'pop-up' windows in a
;; sane and "modern" way (whatever that means). It goes through great lengths to tame
;; helm, flycheck, help buffers--*even* the beast that is org-mode.
;;
;; Be warned, any of this may break as their respective packages update!

(use-package shackle
  :config
  (shackle-mode 1)
  (setq shackle-rules
        '(;; Plugins
          ("\\` ?\\*[hH]elm.*?\\*\\'" :regexp t :align below  :size 20 :select t)
          (" ?\\*Flycheck.+\\*"       :regexp t :align below  :size 15  :noselect t)
          (" *NeoTree*"                      :align left             :select t)
          ("*evil-registers*"                :align below  :size 0.3)
          ("*quickrun*"                      :align below  :size 15  :noselect t)
          ("*eval*"                          :align below  :size 15)
          ("*esup*"                          :align below  :size 30 :noselect t)
          ("*ert*"                           :align below  :size 20 :noselect t)

          ;; vcs
          ("^\\*git-gutter.+\\*$"  :regexp t :align below  :size 0.4 :noselect t)
          ("*vc-diff*"                       :align below  :size 0.4 :noselect t)
          ("*vc-change-log*"                 :align below            :select t)
          (vc-annotate-mode                  :same t)

          ("*Apropos*"                       :align below  :size 0.3)
          ("*minor-modes*"                   :align below  :size 0.5 :noselect t)

          ;; Org
          ("^\\*Org Src .+\\*$"    :regexp t :align below  :size 0.4 :select t)
          ("^\\*Org-Babel.*\\*$"   :regexp t :align below  :size 0.4)
          ("^\\*Org Agenda.+"      :regexp t :align below  :size 0.4)
          ("*Calendar*"                      :align below  :size 0.4)
          (" *Agenda Commands*"              :align below  :size 30)
          (" *Org todo*"                     :align below  :size 5   :noselect t)
          ("*Org Links*"                     :align below  :size 5)

          ;; Emacs
          ("^\\*.+-Profiler-Report .+\\*$" :regexp t :align below :size 0.3)
          ("*Backtrace*" :align below :size 0.25 :noselect t)
          ("*scratch*"   :align below :size 0.3  :select t)
          ("*Help*"      :align below :size 25 :select t)
          ("*Messages*"  :align below :size 20 :select t)
          ("*Completions*" :align below :size 20 :noselect t)
          (debugger-mode :align below :size 0.25 :noselect t)
          (compilation-mode :noselect t)

          ;; REPLs
          ((:custom (lambda (b &rest _)
                      (when (featurep 'repl-toggle)
                        (when (string-prefix-p "*" (buffer-name (get-buffer b)))
                          (with-current-buffer b repl-p)))))
           :popup t :align below :size 16)))

  (after! ert
    (add-hook! 'ert-results-mode-hook 'narf|hide-mode-line)
    (map! (:map ert-results-mode-map
            [escape]   'quit-window
            "<escape>" 'quit-window)))

  (after! help-mode
    ;; So that help buffer links do not open in the help popup, we need to redefine these
    ;; three button types to use `switch-to-buffer-other-window' rather than
    ;; `pop-to-buffer'. Instead of lambas these help-functions should be function symbols,
    ;; so that we could advise those instead of clumsify redefine these button types.
    (define-button-type 'help-function-def
      :supertype 'help-xref
      'help-function (lambda (fun file)
                       (require 'find-func)
                       (when (eq file 'C-source)
                         (setq file (help-C-file-name (indirect-function fun) 'fun)))
                       (let ((location
                              (find-function-search-for-symbol fun nil file)))
                         (switch-to-buffer-other-window (car location))
                         (if (cdr location)
                             (goto-char (cdr location))
                           (message "Unable to find location in file"))))
      'help-echo (purecopy "mouse-2, RET: find function's definition"))

    (define-button-type 'help-variable-def
      :supertype 'help-xref
      'help-function (lambda (var &optional file)
                       (when (eq file 'C-source)
                         (setq file (help-C-file-name var 'var)))
                       (let ((location (find-variable-noselect var file)))
                         (switch-to-buffer-other-window (car location))
                         (if (cdr location)
                             (goto-char (cdr location))
                           (message "Unable to find location in file"))))
      'help-echo (purecopy "mouse-2, RET: find variable's definition"))

    (define-button-type 'help-face-def
      :supertype 'help-xref
      'help-function (lambda (fun file)
                       (require 'find-func)
                       (let ((location
                              (find-function-search-for-symbol fun 'defface file)))
                         (switch-to-buffer-other-window (car location))
                         (if (cdr location)
                             (goto-char (cdr location))
                           (message "Unable to find location in file"))))
      'help-echo (purecopy "mouse-2, RET: find face's definition")))

  (after! helm
    ;; This is a good alternative to either popwin or shackle, specifically for helm. If
    ;; either fail me (for the last time), this is where I'll turn.
    ;;(add-to-list 'display-buffer-alist
    ;;             `(,(rx bos "*helm" (* not-newline) "*" eos)
    ;;               (display-buffer-in-side-window)
    ;;               (inhibit-same-window . t)
    ;;               (window-height . 0.4)))

    ;; Helm tries to clean up after itself, but shackle has already done this. This fixes
    ;; that. To reproduce, add a helm rule in `shackle-rules', open two splits
    ;; side-by-side, move to the buffer on the right and invoke helm. It will close all
    ;; but the left-most buffer.
    (setq-default helm-split-window-in-side-p t))

  (after! helm-swoop
    (setq helm-swoop-split-window-function (lambda ($buf) (narf/popup-buffer $buf))))

  (after! helm-ag
    ;; Helm-ag needs a little coaxing for it to cooperate with shackle. Mostly to prevent
    ;; it from switching between windows and buffers.
    (defadvice helm-ag--edit-abort (around helm-ag-edit-abort-popup-compat activate)
      (cl-letf (((symbol-function 'select-window) 'ignore)) ad-do-it)
      (narf/popup-close nil t t))
    (defadvice helm-ag--edit-commit (around helm-ag-edit-commit-popup-compat activate)
      (cl-letf (((symbol-function 'select-window) 'ignore)) ad-do-it)
      (narf/popup-close nil t t))
    (defadvice helm-ag--edit (around helm-ag-edit-popup-compat activate)
      (cl-letf (((symbol-function 'other-window) 'ignore)
                ((symbol-function 'switch-to-buffer) 'narf/popup-buffer))
        ad-do-it)))

  (after! quickrun
    ;; This allows us to run code several times in a row without having to close the popup
    ;; window and move back to the code buffer.
    (defun narf*quickrun-close-popup (&optional _ _ _ _)
      (let* ((buffer (get-buffer quickrun/buffer-name))
             (window (and buffer (get-buffer-window buffer))))
        (when buffer
          (shut-up! (quickrun/kill-running-process))
          (narf/popup-close window nil t))))
    (advice-add 'quickrun :before 'narf*quickrun-close-popup)
    (advice-add 'quickrun-region :before 'narf*quickrun-close-popup)

    ;; Turns on `nlinum-mode', and ensures window is scrolled to EOF
    (defun narf|quickrun-after-run ()
      (let ((window (get-buffer-window quickrun/buffer-name)))
        (with-selected-window window
          (make-variable-buffer-local 'nlinum-format)
          (setq nlinum-format "%3d ")
          (narf|nlinum-enable)
          (narf|hide-mode-line)
          (let* ((lines (count-lines (point-min) (point-max)))
                 (act-lines (max 5 (min 30 lines))))
            (set-window-start window (evil-line-position (+ 2 (- lines act-lines))))
            (evil-resize-window act-lines)))))
    (add-hook 'quickrun-after-run-hook 'narf|quickrun-after-run)

    ;; I let `narf|quickrun-after-run' handle scrolling, so quickrun shouldn't have to!
    (advice-add 'quickrun/recenter :override 'ignore))

  (after! repl-toggle
    (map! :map repl-toggle-mode-map
          "ESC ESC" 'narf/popup-close))

  (add-hook! org-load
    ;; This ensures org-src-edit yields control of its buffer to shackle.
    (defun org-src-switch-to-buffer (buffer context)
      (pop-to-buffer buffer))

    ;; And these for org-todo, org-link and agenda
    (defun org-pop-to-buffer-same-window (&optional buffer-or-name norecord label)
      "Pop to buffer specified by BUFFER-OR-NAME in the selected window."
      (display-buffer buffer-or-name))

    (defun org-switch-to-buffer-other-window (&rest args)
      (mapc (lambda (b)
              (let ((buf (if (stringp b) (get-buffer-create b) b)))
                (pop-to-buffer buf t t)))
            args))

    ;; Taming Org-agenda!
    (defun narf/org-agenda-quit ()
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

    (map! :map org-agenda-mode-map
          :e "<escape>" 'narf/org-agenda-quit
          :e "ESC" 'narf/org-agenda-quit
          :e [escape] 'narf/org-agenda-quit
          "q" 'narf/org-agenda-quit
          "Q" 'narf/org-agenda-quit))

  (after! flycheck
    (map! :map flycheck-error-list-mode-map
          :n "q" 'narf/popup-close
          :n [escape] 'narf/popup-close))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defvar narf-popup-windows '()
    "A list of windows that have been opened via shackle. Do not touch this!")

  (defun narf*popup-add (&rest _)
    (add-to-list 'narf-popup-windows (get-buffer-window shackle-last-buffer)))
  (advice-add 'shackle-display-buffer :after 'narf*popup-add)

  (defun narf--popup-remove (window)
    (setq narf-popup-windows (delete window narf-popup-windows)))

  (defun narf/popup-p (&optional window)
    "Whether WINDOW is a shackle popup window or not."
    (and narf-popup-windows
         (-any? (lambda (w)
                  (if (window-live-p w) t (narf--popup-remove w) nil))
                narf-popup-windows)
         (if window
             (-any? (lambda (w) (eq window w)) narf-popup-windows)
           t)))

  (defun narf/popup-buffer (buffer &optional plist)
    "Display BUFFER in a shackle popup."
    (let ((buffer-name (if (stringp buffer) buffer (buffer-name buffer))))
      (shackle-display-buffer (get-buffer-create buffer-name)
                              nil (or plist (shackle-match buffer-name)))))

  (defun narf/popup-close (&optional window dont-kill dont-close-all)
    "Find and close the currently active popup (if available)."
    (interactive)
    (when (not window)
      (if (narf/popup-p (selected-window))
          (setq window (selected-window))
        (unless dont-close-all
          (narf/popup-close-all dont-kill))))
    (when (and window (window-live-p window))
      ;; REPL buffer
      (cond ((and (derived-mode-p 'comint-mode)
                  (featurep 'repl-toggle)
                  repl-toggle-mode)
             (setq rtog/--last-buffer nil))
            ((eq major-mode 'messages-buffer-mode)
             (bury-buffer)
             (setq dont-kill t)))
      (narf--popup-remove window)
      (unless dont-kill
        (kill-buffer (window-buffer window)))
      (delete-window window)))

  (defun narf/popup-close-all (&optional dont-kill-buffers)
    "Closes all popup windows (and kills the buffers if DONT-KILL-BUFFERS is non-nil)"
    (interactive)
    (mapc (lambda (w) (narf/popup-close w dont-kill-buffers))
          narf-popup-windows)
    (setq narf-popup-windows nil))

  ;;
  (defun narf/popup-toggle ()
    "Toggles the popup window, reopening the last popup (if available)."
    (interactive)
    (if (narf/popup-p)
        (narf/popup-close t)
      (narf/popup-last-buffer)))

  (defun narf/popup-last-buffer ()
    "Pop up the last popup buffer."
    (interactive)
    (if shackle-last-buffer
        (narf/popup-buffer shackle-last-buffer)
      (narf/popup-messages)))

  (defun narf/popup-messages ()
    "Pop up the *Messages* buffer."
    (interactive)
    (narf/popup-buffer "*Messages*")
    (with-current-buffer "*Messages*"
      (narf|hide-mode-line)
      (goto-char (point-max)))))

(provide 'core-popup)
;;; core-popup.el ends here
