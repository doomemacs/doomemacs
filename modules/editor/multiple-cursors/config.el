;;; editor/multiple-cursors/config.el -*- lexical-binding: t; -*-

(defvar +multiple-cursors-evil-mc-ex-global t
  "TODO")

(defvar +multiple-cursors-evil-mc-ex-case nil
  "TODO")


;;
;;; Packages

(use-package! evil-multiedit
  :when (modulep! :editor evil)
  :defer t)


(use-package! iedit
  :when (modulep! :completion vertico)
  :defer t
  :init
  ;; Fix conflict with embark.
  (setq iedit-toggle-key-default nil))


(use-package! evil-mc
  :when (modulep! :editor evil)
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
  ;; HACK evil-mc's design is bizarre. Its variables and hooks are lazy loaded
  ;;   rather than declared at top-level, some hooks aren't defined or
  ;;   documented, it's a bit initializer-function drunk, and its minor modes
  ;;   are intended to be perpetually active -- even when no cursors are active
  ;;   (causing #6021). I undo all of that here.
  (evil-mc-define-vars)
  (evil-mc-initialize-vars)
  (add-hook 'evil-mc-before-cursors-created #'evil-mc-pause-incompatible-modes)
  (add-hook 'evil-mc-before-cursors-created #'evil-mc-initialize-active-state)
  (add-hook 'evil-mc-after-cursors-deleted  #'evil-mc-teardown-active-state)
  (add-hook 'evil-mc-after-cursors-deleted  #'evil-mc-resume-incompatible-modes)
  (advice-add #'evil-mc-initialize-hooks :override #'ignore)
  (advice-add #'evil-mc-teardown-hooks :override #'evil-mc-initialize-vars)
  (advice-add #'evil-mc-initialize-active-state :before #'turn-on-evil-mc-mode)
  (advice-add #'evil-mc-teardown-active-state :after #'turn-off-evil-mc-mode)
  (defadvice! +multiple-cursors--dont-reinit-vars-a (fn &rest args)
    :around #'evil-mc-mode
    (letf! ((#'evil-mc-initialize-vars #'ignore))
      (apply fn args)))

  ;; REVIEW This is tremendously slow on macos and windows for some reason.
  (setq evil-mc-enable-bar-cursor (featurep :system 'linux))

  (after! smartparens
    ;; Make evil-mc cooperate with smartparens better
    (let ((vars (cdr (assq :default evil-mc-cursor-variables))))
      (unless (memq (car sp--mc/cursor-specific-vars) vars)
        (setcdr (assq :default evil-mc-cursor-variables)
                (append vars sp--mc/cursor-specific-vars)))))

  ;; Whitelist more commands
  (dolist (fn '((backward-kill-word)
                (company-complete-common . evil-mc-execute-default-complete)
                (doom/backward-to-bol-or-indent . evil-mc-execute-default-call)
                (doom/forward-to-last-non-comment-or-eol . evil-mc-execute-default-call)
                ;; :emacs undo
                (undo-fu-only-undo . evil-mc-execute-default-undo)
                (undo-fu-only-redo . evil-mc-execute-default-redo)
                ;; :editor evil
                (evil-delete-back-to-indentation . evil-mc-execute-default-call)
                (evil-escape . evil-mc-execute-default-evil-normal-state)  ; C-g
                (evil-numbers/inc-at-pt-incremental)
                (evil-numbers/dec-at-pt-incremental)
                (evil-digit-argument-or-evil-beginning-of-visual-line
                 (:default . evil-mc-execute-default-call)
                 (visual . evil-mc-execute-visual-call))
                ;; :tools eval
                (+eval:replace-region . +multiple-cursors-execute-default-operator-fn)
                ;; :lang ess
                (ess-smart-comma . evil-mc-execute-call)
                ;; :lang org
                (evil-org-delete . evil-mc-execute-default-evil-delete)))
    (setf (alist-get (car fn) evil-mc-custom-known-commands)
          (if (and (cdr fn) (listp (cdr fn)))
              (cdr fn)
            (list (cons :default
                        (or (cdr fn)
                            #'evil-mc-execute-default-call-with-count))))))

  ;; HACK Allow these commands to be repeated by prefixing them with a numerical
  ;;      argument. See gabesoft/evil-mc#110
  (defadvice! +multiple-cursors--make-repeatable-a (fn)
    :around '(evil-mc-make-and-goto-first-cursor
              evil-mc-make-and-goto-last-cursor
              evil-mc-make-and-goto-prev-cursor
              evil-mc-make-and-goto-next-cursor
              evil-mc-skip-and-goto-prev-cursor
              evil-mc-skip-and-goto-next-cursor
              evil-mc-make-and-goto-prev-match
              evil-mc-make-and-goto-next-match
              evil-mc-skip-and-goto-prev-match
              evil-mc-skip-and-goto-next-match)
    (dotimes (i (if (integerp current-prefix-arg) current-prefix-arg 1))
      (funcall fn)))

  ;; If we're entering insert mode, it's a good bet that we want to start using
  ;; our multiple cursors
  (add-hook 'evil-insert-state-entry-hook #'evil-mc-resume-cursors)

  (pushnew! evil-mc-incompatible-minor-modes
            ;; evil-escape's escape key leaves behind extraneous characters
            'evil-escape-mode
            ;; Lispy commands don't register on more than 1 cursor. Lispyville
            ;; is fine though.
            'lispy-mode)

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
        :nv "g." nil
        :nv "C-n" #'evil-mc-make-and-goto-next-cursor
        :nv "C-S-n" #'evil-mc-make-and-goto-last-cursor
        :nv "C-p" #'evil-mc-make-and-goto-prev-cursor
        :nv "C-S-p" #'evil-mc-make-and-goto-first-cursor))


(after! multiple-cursors-core
  (setq mc/list-file (file-name-concat doom-profile-data-dir "mc-lists.el"))

  ;; Can't use `mc/cmds-to-run-once' because mc-lists.el overwrites it
  (add-to-list 'mc--default-cmds-to-run-once 'swiper-mc)

  ;; TODO multiple-cursors config for Emacs users?

  ;; mc doesn't play well with evil, this attempts to assuage some of its
  ;; problems so that any plugins that depend on multiple-cursors (which I have
  ;; no control over) can still use it in relative safety.
  (when (modulep! :editor evil)
    (evil-define-key* '(normal emacs) mc/keymap [escape] #'mc/keyboard-quit)

    (defvar-local +mc--compat-evil-prev-state nil)
    (defvar-local +mc--compat-mark-was-active nil)

    (add-hook! 'multiple-cursors-mode-enabled-hook
      (defun +multiple-cursors-compat-switch-to-emacs-state-h ()
        (when (and (bound-and-true-p evil-local-mode)
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
              (cl-case +mc--compat-evil-prev-state
                ;; For `evil-multiedit', marked occurrences aren't saved after
                ;; exiting mc, so we should return to normal state anyway
                ((normal visual multiedit multiedit-insert)
                 (evil-force-normal-state))
                (t (message "Don't know how to handle previous state: %S"
                            +mc--compat-evil-prev-state)))
            (setq +mc--compat-evil-prev-state nil)
            (setq +mc--compat-mark-was-active nil)))))

    ;; When running edit-lines, point will return (position + 1) as a result of
    ;; how evil deals with regions
    (defadvice! +multiple--cursors-adjust-mark-for-evil-a (&rest _)
      :before #'mc/edit-lines
      (when (and (bound-and-true-p evil-local-mode)
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
