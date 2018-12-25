;;; editor/multiple-cursors/config.el -*- lexical-binding: t; -*-

(def-package! evil-mc
  :when (featurep! :feature evil)
  :commands (evil-mc-make-cursor-here evil-mc-make-all-cursors
             evil-mc-undo-all-cursors evil-mc-pause-cursors
             evil-mc-resume-cursors evil-mc-make-and-goto-first-cursor
             evil-mc-make-and-goto-last-cursor
             evil-mc-make-cursor-move-next-line
             evil-mc-make-cursor-move-prev-line evil-mc-make-cursor-at-pos
             evil-mc-has-cursors-p evil-mc-make-and-goto-next-cursor
             evil-mc-skip-and-goto-next-cursor evil-mc-make-and-goto-prev-cursor
             evil-mc-skip-and-goto-prev-cursor evil-mc-make-and-goto-next-match
             evil-mc-skip-and-goto-next-match evil-mc-skip-and-goto-next-match
             evil-mc-make-and-goto-prev-match evil-mc-skip-and-goto-prev-match)
  :init
  (defvar evil-mc-key-map (make-sparse-keymap))
  :config
  (global-evil-mc-mode +1)

  (after! smartparens
    ;; Make evil-mc cooperate with smartparens better
    (let ((vars (cdr (assq :default evil-mc-cursor-variables))))
      (unless (memq (car sp--mc/cursor-specific-vars) vars)
        (setcdr (assq :default evil-mc-cursor-variables)
                (append vars sp--mc/cursor-specific-vars)))))

  ;; Add custom commands to whitelisted commands
  (dolist (fn '(doom/backward-to-bol-or-indent doom/forward-to-last-non-comment-or-eol
                doom/backward-kill-to-bol-and-indent delete-char))
    (add-to-list 'evil-mc-custom-known-commands `(,fn (:default . evil-mc-execute-default-call-with-count))))
  ;; Have evil-mc work with explicit `evil-escape' (typically bound to C-g)
  (add-to-list 'evil-mc-custom-known-commands '(evil-escape (:default . evil-mc-execute-default-evil-normal-state)))

  ;; Activate evil-mc cursors upon switching to insert mode
  (defun +evil-mc|resume-cursors () (setq evil-mc-frozen nil))
  (add-hook 'evil-insert-state-entry-hook #'+evil-mc|resume-cursors)

  ;; disable evil-escape in evil-mc; causes unwanted text on invocation
  (add-to-list 'evil-mc-incompatible-minor-modes 'evil-escape-mode nil #'eq)

  (defun +multiple-cursors|escape-multiple-cursors ()
    "Clear evil-mc cursors and restore state."
    (when (evil-mc-has-cursors-p)
      (evil-mc-undo-all-cursors)
      (evil-mc-resume-cursors)
      t))
  (add-hook 'doom-escape-hook #'+multiple-cursors|escape-multiple-cursors)

  ;;
  (evil-add-command-properties '+evil:align :evil-mc t)
  (evil-add-command-properties '+evil:mc :evil-mc t))


(def-package! multiple-cursors
  :defer t
  :config
  (setq mc/list-file (concat doom-etc-dir "mc-lists.el"))

  ;; TODO multiple-cursors config for Emacs users?

  ;; mc doesn't play well with evil, this attempts to assuage some of its
  ;; problems so that any plugins that depend on multiple-cursors (which I have
  ;; no control over) can still use it in relative safety.
  (when (featurep! :feature evil)
    (evil-define-key* '(normal emacs) mc/keymap [escape] #'mc/keyboard-quit)

    (defvar +mc--compat-evil-prev-state nil)
    (defvar +mc--compat-mark-was-active nil)

    (defun +multiple-cursors|compat-switch-to-emacs-state ()
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
            (set-mark mark-before)))))
    (add-hook 'multiple-cursors-mode-enabled-hook #'+multiple-cursors|compat-switch-to-emacs-state)

    (defun +multiple-cursors|compat-back-to-previous-state ()
      (when +mc--compat-evil-prev-state
        (unwind-protect
            (case +mc--compat-evil-prev-state
              ((normal visual) (evil-force-normal-state))
              (t (message "Don't know how to handle previous state: %S"
                          +mc--compat-evil-prev-state)))
          (setq +mc--compat-evil-prev-state nil)
          (setq +mc--compat-mark-was-active nil))))
    (add-hook 'multiple-cursors-mode-disabled-hook #'+multiple-cursors|compat-back-to-previous-state)

    ;; When running edit-lines, point will return (position + 1) as a
    ;; result of how evil deals with regions
    (defun +multiple-cursors*adjust-mark-for-evil (&rest _)
      (when (and (bound-and-true-p evil-mode)
                 (not (memq evil-state '(insert emacs))))
        (if (> (point) (mark))
            (goto-char (1- (point)))
          (push-mark (1- (mark))))))
    (advice-add #'mc/edit-lines :before #'+multiple-cursors*adjust-mark-for-evil)

    (defun +multiple-cursors|evil-compat-rect-switch-state ()
      (if rectangular-region-mode
          (+multiple-cursors|compat-switch-to-emacs-state)
        (setq +mc--compat-evil-prev-state nil)))
    (add-hook 'rectangular-region-mode-hook '+multiple-cursors|evil-compat-rect-switch-state)

    (defvar mc--default-cmds-to-run-once nil)))
