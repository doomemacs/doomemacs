;;; editor/multiple-cursors/config.el -*- lexical-binding: t; -*-

(defvar +multiple-cursors-evil-mc-ex-global t
  "TODO")

(defvar +multiple-cursors-evil-mc-ex-case nil
  "TODO")


;;
;;; Packages

(use-package! evil-multiedit
  :when (featurep! :editor evil)
  :defer t
  :config
  (map! :map (evil-multiedit-state-map evil-multiedit-insert-state-map)
        "C-n" #'evil-multiedit-next
        "C-p" #'evil-multiedit-prev))


(use-package! evil-mc
  :when (featurep! :editor evil)
  :commands (evil-mc-make-cursor-here
             evil-mc-make-all-cursors
             evil-mc-undo-all-cursors
             evil-mc-pause-cursors
             evil-mc-resume-cursors
             evil-mc-make-and-goto-first-cursor
             evil-mc-make-and-goto-last-cursor
             evil-mc-make-cursor-in-visual-selection-beg
             evil-mc-make-cursor-in-visual-selection-end
             evil-mc-make-cursor-move-next-line
             evil-mc-make-cursor-move-prev-line
             evil-mc-make-cursor-at-pos
             evil-mc-has-cursors-p
             evil-mc-make-and-goto-next-cursor
             evil-mc-skip-and-goto-next-cursor
             evil-mc-make-and-goto-prev-cursor
             evil-mc-skip-and-goto-prev-cursor
             evil-mc-make-and-goto-next-match
             evil-mc-skip-and-goto-next-match
             evil-mc-skip-and-goto-next-match
             evil-mc-make-and-goto-prev-match
             evil-mc-skip-and-goto-prev-match)
  :init
  ;; The included keybindings are too imposing and are likely to cause
  ;; conflicts, so we'll set them ourselves.
  (defvar evil-mc-key-map (make-sparse-keymap))

  :config
  (global-evil-mc-mode +1)

  ;; REVIEW This is tremendously slow on macos and windows for some reason.
  (setq evil-mc-enable-bar-cursor (not (or IS-MAC IS-WINDOWS)))

  (after! smartparens
    ;; Make evil-mc cooperate with smartparens better
    (let ((vars (cdr (assq :default evil-mc-cursor-variables))))
      (unless (memq (car sp--mc/cursor-specific-vars) vars)
        (setcdr (assq :default evil-mc-cursor-variables)
                (append vars sp--mc/cursor-specific-vars)))))

  ;; Whitelist more commands
  (dolist (fn '((delete-char)
                (backward-kill-word)
                (company-complete-common . evil-mc-execute-default-complete)
                (doom/backward-to-bol-or-indent . evil-mc-execute-default-call)
                (doom/forward-to-last-non-comment-or-eol . evil-mc-execute-default-call)
                (evil-delete-back-to-indentation . evil-mc-execute-default-call)
                ;; Have evil-mc work with explicit `evil-escape' (on C-g)
                (evil-escape . evil-mc-execute-default-evil-normal-state)
                ;; Add `evil-org' support
                (evil-org-delete . evil-mc-execute-default-evil-delete)
                (evil-org-delete-char . evil-mc-execute-default-evil-delete)
                (evil-org-delete-backward-char . evil-mc-execute-default-evil-delete)
                ;; `evil-numbers'
                (evil-numbers/inc-at-pt-incremental)
                (evil-numbers/dec-at-pt-incremental)))
    (cl-pushnew `(,(car fn) (:default . ,(or (cdr fn) #'evil-mc-execute-default-call-with-count)))
                evil-mc-custom-known-commands
                :test #'eq
                :key #'car))

  ;; If we're entering insert mode, it's a good bet that we want to start using
  ;; our multiple cursors
  (add-hook 'evil-insert-state-entry-hook #'evil-mc-resume-cursors)

  ;; evil-escape's escape key sequence leaves behind extraneous characters
  (cl-pushnew 'evil-escape-mode evil-mc-incompatible-minor-modes)

  (add-hook! 'doom-escape-hook
    (defun +multiple-cursors-escape-multiple-cursors-h ()
      "Clear evil-mc cursors and restore state."
      (when (evil-mc-has-cursors-p)
        (evil-mc-undo-all-cursors)
        (evil-mc-resume-cursors)
        t)))

  ;; Forward declare these so that ex completion and evil-mc support is
  ;; recognized before the autoloaded functions are loaded.
  (evil-add-command-properties '+evil:align :evil-mc t)
  (evil-add-command-properties '+multiple-cursors:evil-mc :evil-mc t)

  (map! :map evil-mc-key-map
        :nv "C-n" #'evil-mc-make-and-goto-next-cursor
        :nv "C-N" #'evil-mc-make-and-goto-last-cursor
        :nv "C-p" #'evil-mc-make-and-goto-prev-cursor
        :nv "C-P" #'evil-mc-make-and-goto-first-cursor))


(after! multiple-cursors-core
  (setq mc/list-file (concat doom-etc-dir "mc-lists.el"))

  ;; TODO multiple-cursors config for Emacs users?

  ;; mc doesn't play well with evil, this attempts to assuage some of its
  ;; problems so that any plugins that depend on multiple-cursors (which I have
  ;; no control over) can still use it in relative safety.
  (when (featurep! :editor evil)
    (evil-define-key* '(normal emacs) mc/keymap [escape] #'mc/keyboard-quit)

    (defvar +mc--compat-evil-prev-state nil)
    (defvar +mc--compat-mark-was-active nil)

    (add-hook! 'multiple-cursors-mode-enabled-hook
      (defun +multiple-cursors-compat-switch-to-emacs-state-h ()
        (when (and (bound-and-true-p evil-mode)
                   (not (memq evil-state '(insert emacs))))
          (setq +mc--compat-evil-prev-state evil-state)
          (when (region-active-p)
            (setq +mc--compat-mark-was-active t))
          (let ((mark-before (mark))
                (point-before (point)))
            (evil-emacs-state 1)
            (when (or +mc--compat-mark-was-active (region-active-p))
              (goto-char point-before)
              (set-mark mark-before))))))

    (add-hook! 'multiple-cursors-mode-disabled-hook
      (defun +multiple-cursors-compat-back-to-previous-state-h ()
        (when +mc--compat-evil-prev-state
          (unwind-protect
              (case +mc--compat-evil-prev-state
                ((normal visual) (evil-force-normal-state))
                (t (message "Don't know how to handle previous state: %S"
                            +mc--compat-evil-prev-state)))
            (setq +mc--compat-evil-prev-state nil)
            (setq +mc--compat-mark-was-active nil)))))

    ;; When running edit-lines, point will return (position + 1) as a result of
    ;; how evil deals with regions
    (defadvice! +multiple--cursors-adjust-mark-for-evil-a (&rest _)
      :before #'mc/edit-lines
      (when (and (bound-and-true-p evil-mode)
                 (not (memq evil-state '(insert emacs))))
        (if (> (point) (mark))
            (goto-char (1- (point)))
          (push-mark (1- (mark))))))

    (add-hook! 'rectangular-region-mode-hook
      (defun +multiple-cursors-evil-compat-rect-switch-state-h ()
        (if rectangular-region-mode
            (+multiple-cursors-compat-switch-to-emacs-state-h)
          (setq +mc--compat-evil-prev-state nil))))

    (defvar mc--default-cmds-to-run-once nil)))
