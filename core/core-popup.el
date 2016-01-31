;;; core-popup.el --- taming stray windows

(use-package shackle
  :config
  (shackle-mode 1)
  (setq shackle-rules
        '(;; Plugins
          ("\\` ?\\*[hH]elm.*?\\*\\'" :regexp t :align below  :size 20 :select t)
          ("*Flycheck errors*"               :align below  :size 15  :noselect t)
          (" *NeoTree*"                      :align left             :select t)
          ("*evil-registers*"                :align below  :size 0.3)
          ("*quickrun*"                      :align below  :size 15  :noselect t)
          ("*eval*"                          :align below  :size 15)
          ("*esup*"                          :align below  :size 30 :noselect t)

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
          (org-agenda-mode                   :align below  :size 0.4)
          ("*Agenda Commands*"               :align below  :size 0.5)
          (" *Org todo*"                     :align below  :size 5   :noselect t)
          ("*Org Links*"                     :align below  :size 5)

          ;; Emacs
          ("^\\*.+-Profiler-Report .+\\*$" :regexp t :align below :size 0.3)
          ("*Backtrace*" :align below :size 0.25 :noselect t)
          ("*scratch*"   :align below :size 0.3  :select t)
          ("*Help*"      :align below :size 15 :select t)
          ("*Messages*"  :align below :size 20 :select t)
          (debugger-mode :align below :size 0.25 :noselect t)
          (compilation-mode :noselect t)

          ;; REPLs
          ((:custom (lambda (b &rest _)
                      (when (featurep 'repl-toggle)
                        (when (string-prefix-p "*" (buffer-name (get-buffer b)))
                          (with-current-buffer b repl-p)))))
           :popup t :align below :size 12)))

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
    ;; but th left-most buffer.
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

    ;; Turns on `yascroll-bar-mode' and `nlinum-mode', and ensures window is scrolled to
    ;; EOF and that the scrollbar is showing.
    (defun narf|quickrun-after-run ()
      (let ((window (get-buffer-window quickrun/buffer-name)))
        (with-selected-window window
          (yascroll-bar-mode +1)
          (narf|nlinum-enable)
          (setq mode-line-format nil)
          (let* ((lines (count-lines (point-min) (point-max)))
                 (act-lines (max 5 (min 30 lines))))
            (set-window-start window (evil-line-position (+ 2 (- lines act-lines))))
            (evil-resize-window act-lines)
            (yascroll:safe-show-scroll-bar) ; scroll-bar starts hidden, but not anymore!
            ))))
    (add-hook 'quickrun-after-run-hook 'narf|quickrun-after-run)

    ;; I let `narf|quickrun-after-run' handle scrolling, so quickrun shouldn't have to!
    (advice-add 'quickrun/recenter :override 'ignore))


  (after! neotree
    ;; Ever since neotree removed the neo-modern-sidebar option, neotree's buffer-opening
    ;; behavior can't be controlled externally. This fixes that and is surprisingly
    ;; stable!
    (defun neo-global--create-window ()
      "Create global neotree window."
      (let ((window nil)
            (buffer (neo-global--get-buffer t)))
        (narf/popup-buffer buffer)
        (setq window
              (select-window
               (neo-global--get-position-window neo-window-position)))
        (neo-window--init window buffer)
        (neo-global--attach)
        (neo-global--reset-width)
        window)))

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
            args)))

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
    (and narf-popup-windows
         (-any? (lambda (w)
                  (if (window-live-p w) t (narf--popup-remove w) nil))
                narf-popup-windows)
         (if window
             (-any? (lambda (w) (eq window w)) narf-popup-windows)
           t)))

  (defun narf/popup-buffer (buffer &optional plist)
    (let ((buffer-name (if (stringp buffer) buffer (buffer-name buffer))))
      (shackle-display-buffer (get-buffer-create buffer-name)
                              nil (or plist (shackle-match buffer-name)))))

  (defun narf/popup-close (&optional window dont-kill dont-close-all)
    (interactive)
    (when (not window)
      (if (narf/popup-p (selected-window))
          (setq window (selected-window))
        (unless dont-close-all
          (narf-popup-close-all dont-kill))))
    (when (and window (window-live-p window))
      ;; REPL buffer
      (cond ((and (derived-mode-p 'comint-mode)
                  (featurep 'repl-toggle)
                  repl-toggle-mode)
             (setq rtog/--last-buffer nil
                   narf-repl-buffer nil))
            ((eq major-mode 'messages-buffer-mode)
             (bury-buffer)
             (setq dont-kill t)))
      (narf--popup-remove window)
      (unless dont-kill
        (kill-buffer (window-buffer window)))
      (delete-window window)))

  (defun narf-popup-close-all (&optional dont-kill-buffers)
    (interactive)
    (mapc (lambda (w) (narf/popup-close w dont-kill-buffers))
          narf-popup-windows)
    (setq narf-popup-windows nil))

  ;;;;;

  (defun narf/popup-toggle ()
    (interactive)
    (if (narf/popup-p)
        (narf/popup-close t)
      (narf/popup-last-buffer)))

  (defun narf/popup-last-buffer ()
    (interactive)
    (if shackle-last-buffer
        (narf/popup-buffer shackle-last-buffer)
      (narf/popup-messages)))

  (defun narf/popup-messages ()
    (interactive)
    (narf/popup-buffer "*Messages*")
    (with-current-buffer "*Messages*"
      (setq mode-line-format nil)
      (goto-char (point-max)))))

(provide 'core-popup)
;;; core-popup.el ends here
