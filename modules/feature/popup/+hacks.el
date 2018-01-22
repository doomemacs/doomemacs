;;; feature/popup/+hacks.el -*- lexical-binding: t; -*-

;; What follows are all the hacks needed to get various parts of Emacs and other
;; plugins to cooperate with the popup management system. Essentially, it comes
;; down to:
;;
;; 1. Making plugins that control their own window environment less greedy (e.g.
;;    org agenda, trying to reconfigure the entire frame to pop up one tiny
;;    window).
;; 2. Forcing plugins to use `display-buffer' and `pop-to-buffer' instead of
;;    `switch-to-buffer' (which is unaffected by `display-buffer-alist', which
;;    this module heavily relies on).
;; 3. Closing popups (temporarily) before functions that are highly destructive
;;    to the illusion of popup control get run (with the use of the
;;   `save-popups!' macro).
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
;;

;; Don't try to resize popup windows
(advice-add #'balance-windows :around #'+popup*save)


;;
;; External functions
;;

(after! eshell
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
(after! evil
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
    (let (origin)
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
(after! helpful
  (defun +popup*helpful--navigate (orig-fn &rest args)
    (let (origin)
      (save-popups!
       (apply orig-fn args)
       (setq origin (selected-window))
       (recenter))
      (select-window origin)))
  (advice-add #'helpful--navigate :around #'+popup*helpful--navigate))


;; `neotree'
(after! neotree
  (advice-add #'neo-util--set-window-width :override #'ignore)
  (advice-remove #'balance-windows #'ad-Advice-balance-windows))


;; `org'
(after! org
  (set! :popup "^\\*\\(?:Agenda Com\\|Calendar\\|Org \\(?:Links\\|Export Dispatcher\\|Select\\)\\)"
    '((slot . -1) (vslot . -1) (size . +popup-shrink-to-fit))
    '((transient . 0)))
  (set! :popup "^\\*Org Agenda" '((size . 20)) '((select . t) (transient)))
  (set! :popup "^\\*Org Src"    '((size . 0.3)) '((quit) (select . t)))
  (set! :popup "^CAPTURE.*\\.org$" '((size . 0.2)) '((quit) (select . t)))

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
    (if +popup-mode
        (if (eq org-src-window-setup 'switch-invisibly) ; for internal calls
            (set-buffer buffer)
          (display-buffer buffer))
      (funcall orig-fn buffer context)))
  (advice-add #'org-src-switch-to-buffer :around #'+popup*org-src-pop-to-buffer)
  (setq org-src-window-setup 'other-window)

  ;; Ensure todo, agenda, and other minor popups are delegated to the popup system.
  (defun +popup*org-pop-to-buffer (orig-fn buf &optional norecord)
    "Use `pop-to-buffer' instead of `switch-to-buffer' to open buffer.'"
    (if +popup-mode
        (pop-to-buffer
         (cond ((stringp buf) (get-buffer-create buf))
               ((bufferp buf) buf)
               (t (error "Invalid buffer %s" buf))))
      (funcall orig-fn buf norecord)))
  (advice-add #'org-switch-to-buffer-other-window :around #'+popup*org-pop-to-buffer)

  ;; `org-agenda'
  (setq org-agenda-window-setup 'other-window
        org-agenda-restore-windows-after-quit nil)
  ;; Don't monopolize frame!
  (advice-add #'org-agenda :around #'+popup*suppress-delete-other-windows))


;; `persp-mode'
(progn
  (defun +popup*persp-mode-restore-popups (&rest _)
    "Restore popup windows when loading a perspective from file."
    (dolist (window (window-list))
      (when (+popup-parameter 'popup window)
        (+popup--init window nil))))
  (advice-add #'persp-load-state-from-file :after #'+popup*persp-mode-restore-popups))


;; `multi-term'
(after! multi-term
  (setq multi-term-buffer-name "doom terminal"))


;; `wgrep'
(progn
  ;; close the popup after you're done with a wgrep buffer
  (advice-add #'wgrep-abort-changes :after #'+popup*close)
  (advice-add #'wgrep-finish-edit :after #'+popup*close))


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
