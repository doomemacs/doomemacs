;;; ui/popup/+hacks.el -*- lexical-binding: t; -*-

;; What follows are all the hacks needed to get various parts of Emacs and other
;; plugins to cooperate with the popup management system. Essentially, it comes
;; down to:
;;
;; 1. Making plugins that control their own window environment less greedy (e.g.
;;    org agenda, which tries to reconfigure the entire frame (by deleting all
;;    other windows) just to pop up one tiny window).
;; 2. Forcing plugins to use `display-buffer' and `pop-to-buffer' instead of
;;    `switch-to-buffer' (which is unaffected by `display-buffer-alist', which
;;    this module heavily relies on).
;; 3. Closing popups (temporarily) before functions that are highly destructive
;;    to the illusion of popup control get run (with the use of the
;;    `save-popups!' macro).
;;
;; Keep in mind, all this black magic may break in future updates, and will need
;; to be watched carefully for corner cases. Also, once this file is loaded, its
;; changes are irreversible without restarting Emacs! I don't like it either,
;; but I will address this over time.
;;
;; Hacks should be kept in alphabetical order, named after the feature they
;; modify, and should follow a ;; `package-name' header line.

;;
;; Core functions

;; Don't try to resize popup windows
(advice-add #'balance-windows :around #'+popup*save)


;;
;; External functions

;; `buff-menu'
(define-key Buffer-menu-mode-map (kbd "RET") #'Buffer-menu-other-window)


;; `company'
(progn
  (defun +popup*dont-select-me (orig-fn &rest args)
    (let ((+popup--inhibit-select t))
      (apply orig-fn args)))
  (advice-add #'company-show-doc-buffer :around #'+popup*dont-select-me))


;; `eshell'
(progn
  (setq eshell-destroy-buffer-when-process-dies t)

  ;; When eshell runs a visual command (see `eshell-visual-commands'), it spawns
  ;; a term buffer to run it in, but where it spawns it is the problem...
  (defun +popup*eshell-undedicate-popup (orig-fn &rest args)
    "Force spawned term buffer to share with the eshell popup (if necessary)."
    (when (+popup-window-p)
      (set-window-dedicated-p nil nil)
      (add-transient-hook! #'eshell-query-kill-processes :after
                           (set-window-dedicated-p nil t)))
    (apply orig-fn args))
  (advice-add #'eshell-exec-visual :around #'+popup*eshell-undedicate-popup))


;; `evil'
(progn
  (defun +popup*evil-command-window (hist cmd-key execute-fn)
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

  (defun +popup*evil-command-window-execute ()
    "Execute the command under the cursor in the appropriate buffer, rather than
the command buffer."
    (interactive)
    (let ((result (buffer-substring (line-beginning-position)
                                    (line-end-position)))
          (execute-fn evil-command-window-execute-fn)
          (execute-window (get-buffer-window evil-command-window-current-buffer))
          (popup (selected-window)))
      (if execute-window
          (select-window execute-window)
        (user-error "Originating buffer is no longer active"))
      ;; (kill-buffer "*Command Line*")
      (delete-window popup)
      (funcall execute-fn result)
      (setq evil-command-window-current-buffer nil)))

  ;; Make evil-mode cooperate with popups
  (advice-add #'evil-command-window :override #'+popup*evil-command-window)
  (advice-add #'evil-command-window-execute :override #'+popup*evil-command-window-execute)

  ;; Don't mess with popups
  (advice-add #'+evil--window-swap           :around #'+popup*save)
  (advice-add #'evil-window-move-very-bottom :around #'+popup*save)
  (advice-add #'evil-window-move-very-top    :around #'+popup*save)
  (advice-add #'evil-window-move-far-left    :around #'+popup*save)
  (advice-add #'evil-window-move-far-right   :around #'+popup*save))


;; `help-mode'
(after! help-mode
  (defun doom--switch-from-popup (location)
    (let (origin enable-local-variables)
      (save-popups!
       (switch-to-buffer (car location) nil t)
       (if (not (cdr location))
           (message "Unable to find location in file")
         (goto-char (cdr location))
         (recenter)
         (setq origin (selected-window))))
      (select-window origin)))

  ;; Help buffers use `pop-to-window' to decide where to open followed links,
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


;; `helpful'
(progn
  ;; Open link in origin window (non-popup) instead of inside the popup window.
  (defun +popup*helpful--navigate (button)
    (let ((path (substring-no-properties (button-get button 'path)))
          enable-local-variables
          origin)
      (save-popups!
       (find-file path)
       (-when-let (pos (get-text-property button 'position
                                          (marker-buffer button)))
         (goto-char pos))
       (setq origin (selected-window))
       (recenter))
      (select-window origin)))
  (advice-add #'helpful--navigate :override #'+popup*helpful--navigate))


;; `helm'
(when (featurep! :completion helm)
  (setq helm-default-display-buffer-functions '(+popup-display-buffer-stacked-side-window))

  ;; Fix #897: "cannot open side window" error when TAB-completing file links
  (defun +popup*hide-org-links-popup (orig-fn &rest args)
    (cl-letf* ((old-org-completing-read (symbol-function 'org-completing-read))
               ((symbol-function 'org-completing-read)
                (lambda (&rest args)
                  (when-let* ((win (get-buffer-window "*Org Links*")))
                    ;; While helm opened as a popup, helm commands will mistaken
                    ;; the *Org Links* popup for the "originated window", and
                    ;; try to manipulate it, but since that is a popup too (as
                    ;; is a dedicated side window), Emacs errors and complains
                    ;; it can't do that. So we get rid of it.
                    (delete-window win)
                    (get-buffer-create "*Org Links*"))
                  (apply old-org-completing-read args))))
      (apply orig-fn args)))
  (advice-add #'org-insert-link :around #'+popup*hide-org-links-popup)

  ;; Fix left-over popup window when closing persistent help for `helm-M-x'
  (defun +popup*helm-elisp--persistent-help (candidate _fun &optional _name)
    (let (win)
      (when (and (helm-attr 'help-running-p)
                 (string= candidate (helm-attr 'help-current-symbol))
                 (setq win (get-buffer-window (get-buffer (help-buffer)))))
        (delete-window win))))
  (advice-add #'helm-elisp--persistent-help :before #'+popup*helm-elisp--persistent-help)

  ;; `helm-ag'
  (defun +helm*pop-to-buffer (orig-fn &rest args)
    (pop-to-buffer
     (save-window-excursion (apply orig-fn args)
                            (current-buffer))))
  (advice-add #'helm-ag--edit :around #'+helm*pop-to-buffer))


;; `ibuffer'
(setq ibuffer-use-other-window t)


;; `Info'
(defun +popup*switch-to-info-window (&rest _)
  (when-let* ((win (get-buffer-window "*info*")))
    (when (+popup-window-p win)
      (select-window win))))
(advice-add #'info-lookup-symbol :after #'+popup*switch-to-info-window)


;; `multi-term'
(setq multi-term-buffer-name "doom terminal")


;; `neotree'
(after! neotree
  (advice-add #'neo-util--set-window-width :override #'ignore)
  (advice-remove #'balance-windows #'ad-Advice-balance-windows))


;; `org'
(after! org
  (defvar +popup--disable-internal nil)
  ;; Org has a scorched-earth window management system I'm not fond of. i.e. it
  ;; kills all windows and monopolizes the frame. No thanks. We can do better
  ;; ourselves.
  (defun +popup*suppress-delete-other-windows (orig-fn &rest args)
    (if +popup-mode
        (cl-letf (((symbol-function 'delete-other-windows)
                   (symbol-function 'ignore)))
          (apply orig-fn args))
      (apply orig-fn args)))
  (advice-add #'org-add-log-note :around #'+popup*suppress-delete-other-windows)
  (advice-add #'org-capture-place-template :around #'+popup*suppress-delete-other-windows)
  (advice-add #'org-export--dispatch-ui :around #'+popup*suppress-delete-other-windows)

  (defun +popup*org-src-pop-to-buffer (orig-fn buffer context)
    "Hand off the src-block window to the popup system by using `display-buffer'
instead of switch-to-buffer-*."
    (if (and (eq org-src-window-setup 'popup-window)
             +popup-mode)
        (pop-to-buffer buffer)
      (funcall orig-fn buffer context)))
  (advice-add #'org-src-switch-to-buffer :around #'+popup*org-src-pop-to-buffer)
  (setq org-src-window-setup 'popup-window)

  ;; Ensure todo, agenda, and other minor popups are delegated to the popup system.
  (defun +popup*org-pop-to-buffer (orig-fn buf &optional norecord)
    "Use `pop-to-buffer' instead of `switch-to-buffer' to open buffer.'"
    (if +popup-mode
        (pop-to-buffer buf nil norecord)
      (funcall orig-fn buf norecord)))
  (advice-add #'org-switch-to-buffer-other-window :around #'+popup*org-pop-to-buffer)

  ;; `org-agenda'
  (setq org-agenda-window-setup 'other-window
        org-agenda-restore-windows-after-quit nil)
  ;; Don't monopolize frame!
  (defun +popup*org-agenda-suppress-delete-other-windows (orig-fn &rest args)
    (cond ((not +popup-mode)
           (apply orig-fn args))
          ((eq org-agenda-window-setup 'popup-window)
           (let (org-agenda-restore-windows-after-quit)
             (cl-letf (((symbol-function 'delete-other-windows)
                        (symbol-function 'ignore)))
               (apply orig-fn args))))
          ((memq org-agenda-window-setup '(current-window other-window))
           (with-popup-rules! nil
             (cl-letf (((symbol-function 'delete-other-windows)
                        (symbol-function 'ignore)))
               (apply orig-fn args))))
          ((with-popup-rules! nil
             (apply orig-fn args)))))
  (advice-add #'org-agenda-prepare-window :around #'+popup*org-agenda-suppress-delete-other-windows))


;; `persp-mode'
(progn
  (defun +popup*persp-mode-restore-popups (&rest _)
    "Restore popup windows when loading a perspective from file."
    (dolist (window (window-list))
      (when (+popup-parameter 'popup window)
        (+popup--init window nil))))
  (advice-add #'persp-load-state-from-file :after #'+popup*persp-mode-restore-popups))


;; `pdf-tools'
(after! pdf-tools
  (setq tablist-context-window-display-action
        '((+popup-display-buffer-stacked-side-window)
          (side . left)
          (slot . 2)
          (window-height . 0.3)
          (inhibit-same-window . t))
        pdf-annot-list-display-buffer-action
        '((+popup-display-buffer-stacked-side-window)
          (side . left)
          (slot . 3)
          (inhibit-same-window . t)))

  (add-hook 'pdf-annot-list-mode-hook #'hide-mode-line-mode)
  (set-popup-rule! "\\(^\\*Contents\\|'s annots\\*$\\)" :ignore t))


;; `wgrep'
(progn
  ;; close the popup after you're done with a wgrep buffer
  (advice-add #'wgrep-abort-changes :after #'+popup*close)
  (advice-add #'wgrep-finish-edit :after #'+popup*close))


;; `which-key'
(after! which-key
  (when (eq which-key-popup-type 'side-window)
    (setq which-key-popup-type 'custom
          which-key-custom-popup-max-dimensions-function (lambda (_) (which-key--side-window-max-dimensions))
          which-key-custom-hide-popup-function #'which-key--hide-buffer-side-window
          which-key-custom-show-popup-function
          (lambda (act-popup-dim)
            (cl-letf (((symbol-function 'display-buffer-in-side-window)
                       (lambda (buffer alist)
                         (+popup-display-buffer-stacked-side-window
                          buffer (append '((vslot . -9999)) alist)))))
              (which-key--show-buffer-side-window act-popup-dim))))))


;; `windmove'
(progn
  ;; Users should be about to hop into popups easily, but Elisp shouldn't.
  (defun doom*ignore-window-parameters (orig-fn &rest args)
    "Allow *interactive* window moving commands to traverse popups."
    (cl-letf (((symbol-function #'windmove-find-other-window)
               (lambda (dir &optional arg window)
                 (window-in-direction
                  (pcase dir (`up 'above) (`down 'below) (_ dir))
                  window (bound-and-true-p +popup-mode) arg windmove-wrap-around t))))
      (apply orig-fn args)))
  (advice-add #'windmove-up :around #'doom*ignore-window-parameters)
  (advice-add #'windmove-down :around #'doom*ignore-window-parameters)
  (advice-add #'windmove-left :around #'doom*ignore-window-parameters)
  (advice-add #'windmove-right :around #'doom*ignore-window-parameters))
