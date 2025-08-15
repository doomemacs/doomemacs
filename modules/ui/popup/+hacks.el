;;; ui/popup/+hacks.el -*- lexical-binding: t; -*-

;; What follows are all the hacks needed to get various parts of Emacs and other
;; plugins to cooperate with the popup management system. Essentially, it comes
;; down to:
;;
;; 1. Making plugins that control their own window environment less greedy (e.g.
;;    org agenda, which tries to reconfigure the entire frame by deleting all
;;    other windows just to pop up one tiny window).
;; 2. Forcing plugins to use `display-buffer' and `pop-to-buffer' instead of
;;    `switch-to-buffer' (which is unaffected by `display-buffer-alist', which
;;    we must rely on, heavily).
;; 3. Closing popups (temporarily) before functions that are highly destructive
;;    to the illusion of popup control get run (with the use of the
;;    `save-popups!' macro).
;;
;; Keep in mind, all this black magic may break in future updates, and will need
;; to be watched carefully for corner cases. Also, once this file is loaded,
;; many of its changes are irreversible without restarting Emacs! I don't like
;; it either, but I will address this over time.
;;
;; Hacks should be kept in alphabetical order, named after the feature they
;; modify, and should follow a ;;;## package-name header line (if not using
;; `after!' or `use-package!').

;;
;;; Core functions

(defadvice! +popup--make-case-sensitive-a (fn &rest args)
  "Make regexps in `display-buffer-alist' case-sensitive.

To reduce fewer edge cases and improve performance when `display-buffer-alist'
grows larger."
  :around #'display-buffer-assq-regexp
  (let (case-fold-search)
    (apply fn args)))

;; Don't try to resize popup windows
(advice-add #'balance-windows :around #'+popup-save-a)

(defun +popup/quit-window (&optional arg)
  "The regular `quit-window' sometimes kills the popup buffer and switches to a
buffer that shouldn't be in a popup. We prevent that by remapping `quit-window'
to this commmand."
  (interactive "P")
  (let ((orig-buffer (current-buffer)))
    (quit-window arg)
    (when (and (eq orig-buffer (current-buffer))
               (+popup-buffer-p))
      (+popup/close nil 'force))))
(define-key +popup-buffer-mode-map [remap quit-window] #'+popup/quit-window)


;;
;;; External functions

;;;###package buff-menu
(define-key Buffer-menu-mode-map (kbd "RET") #'Buffer-menu-other-window)


;;;###package company
(defadvice! +popup--dont-select-me-a (fn &rest args)
  :around #'company-show-doc-buffer
  (let ((+popup--inhibit-select t))
    (apply fn args)))


;;;###package compile
(defadvice! +popup--compilation-goto-locus-a (fn &rest args)
  "Fix links in popup compilation buffers creating a new window each time they
were followed."
  :around #'compilation-goto-locus
  (letf! (defun pop-to-buffer (buffer &optional action norecord)
           (let ((pop-up-windows (not (+popup-buffer-p (current-buffer)))))
             (funcall pop-to-buffer buffer action norecord)))
    (apply fn args)))


;;;###package eshell
(progn
  (setq eshell-destroy-buffer-when-process-dies t)

  ;; When eshell runs a visual command (see `eshell-visual-commands'), it spawns
  ;; a term buffer to run it in, but where it spawns it is the problem...
  (defadvice! +popup--eshell-undedicate-popup (&rest _)
    "Force spawned term buffer to share with the eshell popup (if necessary)."
    :before #'eshell-exec-visual
    (when (+popup-window-p)
      (set-window-dedicated-p nil nil)
      (add-transient-hook! #'eshell-query-kill-processes :after
                           (set-window-dedicated-p nil t)))))


;;;###package evil
(progn
  (defadvice! +popup--evil-command-window-execute-a ()
    "Execute the command under the cursor in the appropriate buffer, rather than
the command buffer."
    :override #'evil-command-window-execute
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

  ;; Don't mess with popups
  (advice-add #'+evil--window-swap           :around #'+popup-save-a)
  (advice-add #'evil-window-move-very-bottom :around #'+popup-save-a)
  (advice-add #'evil-window-move-very-top    :around #'+popup-save-a)
  (advice-add #'evil-window-move-far-left    :around #'+popup-save-a)
  (advice-add #'evil-window-move-far-right   :around #'+popup-save-a))


(after! help-mode
  (defun +popup--switch-from-popup (location)
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
      (+popup--switch-from-popup (find-function-search-for-symbol fun nil file))))

  (define-button-type 'help-variable-def
    :supertype 'help-xref
    'help-function
    (lambda (var &optional file)
      (when (eq file 'C-source)
        (setq file (help-C-file-name var 'var)))
      (+popup--switch-from-popup (find-variable-noselect var file))))

  (define-button-type 'help-face-def
    :supertype 'help-xref
    'help-function
    (lambda (fun file)
      (require 'find-func)
      (+popup--switch-from-popup (find-function-search-for-symbol fun 'defface file)))))


;;;###package helpful
(defadvice! +popup--helpful-open-in-origin-window-a (button)
  "Open links in non-popup, originating window rather than helpful's window."
  :override #'helpful--navigate
  (let ((path (substring-no-properties (button-get button 'path)))
        enable-local-variables
        origin)
    (save-popups!
     (find-file path)
     (when-let (pos (get-text-property button 'position
                                       (marker-buffer button)))
       (goto-char pos))
     (setq origin (selected-window))
     (recenter))
    (select-window origin)))


;;;###package helm
;;;###package helm-ag
(when (modulep! :completion helm)
  (setq helm-default-display-buffer-functions '(+popup-display-buffer-stacked-side-window-fn))

  ;; Fix #897: "cannot open side window" error when TAB-completing file links
  (defadvice! +popup--helm-hide-org-links-popup-a (fn &rest args)
    :around #'org-insert-link
    (letf! ((defun org-completing-read (&rest args)
              (when-let (win (get-buffer-window "*Org Links*"))
                ;; While helm is opened as a popup, it will mistaken the *Org
                ;; Links* popup for the "originated window", and will target it
                ;; for actions invoked by the user. However, since *Org Links*
                ;; is a popup too (they're dedicated side windows), Emacs
                ;; complains about being unable to split a side window. The
                ;; simple fix: get rid of *Org Links*!
                (delete-window win)
                ;; ...but it must exist for org to clean up later.
                (get-buffer-create "*Org Links*"))
              (apply org-completing-read args)))
      (apply #'funcall-interactively fn args)))

  ;; Fix left-over popup window when closing persistent help for `helm-M-x'
  (defadvice! +popup--helm-elisp--persistent-help-a (candidate _fun &optional _name)
    :before #'helm-elisp--persistent-help
    (let (win)
      (and (helm-attr 'help-running-p)
           (string= candidate (helm-attr 'help-current-symbol))
           (setq win (get-buffer-window (get-buffer (help-buffer))))
           (delete-window win)))))


;;;###package Info
(defadvice! +popup--switch-to-info-window-a (&rest _)
  :after #'info-lookup-symbol
  (when-let (win (get-buffer-window "*info*"))
    (when (+popup-window-p win)
      (select-window win))))


;;;###package latex
(defadvice! +popup--use-popup-window-for-reftex-citation-a (fn &rest args)
  :around #'reftex-do-citation
  (letf! ((#'switch-to-buffer-other-window #'pop-to-buffer))
    (apply fn args)))


(after! org
  (defadvice! +popup--suppress-delete-other-windows-a (fn &rest args)
    "Org has a scorched-earth window management policy I'm not fond of. i.e. it
kills all other windows just so it can monopolize the frame. No thanks. We can
do better."
    :around #'org-add-log-note
    :around #'org-capture-place-template
    :around #'org-export--dispatch-ui
    :around #'org-agenda-get-restriction-and-command
    :around #'org-goto-location
    :around #'org-fast-tag-selection
    :around #'org-fast-todo-selection
    (if +popup-mode
        (letf! ((#'delete-other-windows #'ignore)
                (#'delete-window        #'ignore))
          (apply fn args))
      (apply fn args)))

  (defadvice! +popup--org-fix-goto-a (fn &rest args)
    "`org-goto' uses `with-output-to-temp-buffer' to display its help buffer,
for some reason, which is very unconventional, and so requires these gymnastics
to tame (i.e. to get the popup manager to handle it)."
    :around #'org-goto-location
    (if +popup-mode
        (letf! (defun internal-temp-output-buffer-show (buffer)
                 (let ((temp-buffer-show-function
                        (doom-rpartial #'+popup-display-buffer-stacked-side-window-fn nil)))
                   (with-current-buffer buffer
                     (+popup-buffer-mode +1))
                   (funcall internal-temp-output-buffer-show buffer)))
          (apply fn args))
      (apply fn args)))

  (defadvice! +popup--org-fix-popup-window-shrinking-a (fn &rest args)
    "Hides the mode-line in *Org tags* buffer so you can actually see its
content and displays it in a side window without deleting all other windows.
Ugh, such an ugly hack."
    :around #'org-fast-tag-selection
    :around #'org-fast-todo-selection
    (if +popup-mode
        (letf! ((defun read-char-exclusive (&rest args)
                  (message nil)
                  (apply read-char-exclusive args))
                (defun split-window-vertically (&optional _size)
                  (funcall split-window-vertically (- 0 window-min-height 1)))
                (defun org-fit-window-to-buffer (&optional window max-height min-height shrink-only)
                  (when-let (buf (window-buffer window))
                    (with-current-buffer buf
                      (+popup-buffer-mode)))
                  (when (> (window-buffer-height window)
                           (window-height window))
                    (fit-window-to-buffer window (window-buffer-height window)))))
          (apply fn args))
      (apply fn args)))

  (defadvice! +popup--org-edit-src-exit-a (fn &rest args)
    "If you switch workspaces or the src window is recreated..."
    :around #'org-edit-src-exit
    (let* ((window (selected-window))
           (popup-p (+popup-window-p window)))
      (prog1 (apply fn args)
        (when (and popup-p (window-live-p window))
          (delete-window window))))))

;;;###package org-journal
(defadvice! +popup--use-popup-window-a (fn &rest args)
  :around #'org-journal--search-by-string
  (letf! ((#'switch-to-buffer #'pop-to-buffer))
    (apply fn args)))


;;;###package persp-mode
(defadvice! +popup--persp-mode-restore-popups-a (&rest _)
  "Restore popup windows when loading a perspective from file."
  :after #'persp-load-state-from-file
  (dolist (window (window-list))
    (when (+popup-parameter 'popup window)
      (+popup--init window nil))))


(after! pdf-tools
  (setq tablist-context-window-display-action
        '((+popup-display-buffer-stacked-side-window-fn)
          (side . left)
          (slot . 2)
          (window-height . 0.3)
          (inhibit-same-window . t))
        pdf-annot-list-display-buffer-action
        '((+popup-display-buffer-stacked-side-window-fn)
          (side . left)
          (slot . 3)
          (inhibit-same-window . t))))


;;;###package profiler
(defadvice! +popup--profiler-report-find-entry-in-other-window-a (fn function)
  :around #'profiler-report-find-entry
  (letf! ((#'find-function #'find-function-other-window))
    (funcall fn function)))


;;;###package undo-tree
(defadvice! +popup--use-popup-window-for-undo-tree-visualizer-a (fn &rest args)
  "TODO"
  :around #'undo-tree-visualize
  (if undo-tree-visualizer-diff
      (apply fn args)
    (letf! ((#'switch-to-buffer-other-window #'pop-to-buffer))
      (apply fn args))))

;;;###package wdired
(progn
  ;; close the popup after you're done with a wdired buffer
  (advice-add #'wdired-abort-changes :after #'+popup-close-a)
  (advice-add #'wdired-finish-edit :after #'+popup-close-a))

;;;###package wgrep
(progn
  ;; close the popup after you're done with a wgrep buffer
  (advice-add #'wgrep-abort-changes :after #'+popup-close-a)
  (advice-add #'wgrep-finish-edit :after #'+popup-close-a))


(after! which-key
  (when (eq which-key-popup-type 'side-window)
    (setq which-key-popup-type 'custom
          which-key-custom-popup-max-dimensions-function
          (lambda (_) (which-key--side-window-max-dimensions))
          which-key-custom-hide-popup-function #'which-key--hide-buffer-side-window
          which-key-custom-show-popup-function
          (lambda (act-popup-dim)
            (letf! (defun display-buffer-in-side-window (buffer alist)
                     (+popup-display-buffer-stacked-side-window-fn
                      buffer (append '((vslot . -9999) (select . t)) alist)))
              ;; HACK Fix #2219 where the which-key popup would get cut off.
              (setcar act-popup-dim (1+ (car act-popup-dim)))
              (which-key--show-buffer-side-window act-popup-dim))))))


;;;###package windmove
;; Users should be able to hop into popups easily, but Elisp shouldn't.
(defadvice! +popup--ignore-window-parameters-a (fn &rest args)
  "Allow *interactive* window moving commands to traverse popups."
  :around '(windmove-up windmove-down windmove-left windmove-right)
  (letf! (defun windmove-find-other-window (dir &optional arg window)
           (window-in-direction
            (pcase dir (`up 'above) (`down 'below) (_ dir))
            window (bound-and-true-p +popup-mode) arg windmove-wrap-around t))
    (apply fn args)))
