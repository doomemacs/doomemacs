;;; ui/indent-guides/config.el -*- lexical-binding: t; -*-

(defcustom +indent-guides-inhibit-functions ()
  "A list of predicate functions.

Each function will be run in the context of a buffer where `indent-bars' should
be enabled. If any function returns non-nil, the mode will not be activated."
  :type 'hook
  :group '+indent-guides)


;;
;;; Packages

(use-package! indent-bars
  :unless noninteractive
  :hook ((prog-mode text-mode conf-mode) . +indent-guides-init-maybe-h)
  :init
  (defun +indent-guides-init-maybe-h ()
    "Enable `indent-bars-mode' depending on `+indent-guides-inhibit-functions'."
    (unless (run-hook-with-args-until-success '+indent-guides-inhibit-functions)
      (indent-bars-mode +1)))
  :config
  (setq indent-bars-treesit-support (modulep! :tools tree-sitter)
        indent-bars-prefer-character
        (or
         ;; Bitmaps are far slower on MacOS, inexplicably, but this needs more
         ;; testing to see if it's specific to ns or emacs-mac builds, or is
         ;; just a general MacOS issue.
         (featurep :system 'macos)
         ;; FIX: A bitmap init bug in emacs-pgtk (before v30) could cause
         ;; crashes (see jdtsmith/indent-bars#3).
         (and (featurep 'pgtk)
              (< emacs-major-version 30)))

        ;; Show indent guides starting from the first column.
        indent-bars-starting-column 0
        ;; Make indent guides subtle; the default is too distractingly colorful.
        indent-bars-width-frac 0.15  ; make bitmaps thinner
        indent-bars-color-by-depth nil
        indent-bars-color '(font-lock-comment-face :face-bg nil :blend 0.425)
        ;; Don't highlight current level indentation; it's distracting and is
        ;; unnecessary overhead for little benefit.
        indent-bars-highlight-current-depth nil)

  ;; indent-bars adds this to `enable-theme-functions', which was introduced in
  ;; 29.1, which will be redundant with `doom-load-theme-hook'.
  (unless (boundp 'enable-theme-functions)
    (add-hook 'doom-load-theme-hook #'indent-bars-reset-styles))

  (add-hook! '+indent-guides-inhibit-functions
    ;; Org's virtual indentation messes up indent-guides.
    (defun +indent-guides-in-org-indent-mode-p ()
      (bound-and-true-p org-indent-mode))
    ;; Fix #6438: indent-guides prevent inline images from displaying in ein
    ;; notebooks.
    (defun +indent-guides-in-ein-notebook-p ()
      (and (bound-and-true-p ein:notebook-mode)
           (bound-and-true-p ein:output-area-inlined-images)))
    ;; Don't display indent guides in childframe popups (which are almost always
    ;; used for completion or eldoc popups).
    ;; REVIEW: Swap with `frame-parent' when 27 support is dropped
    (defun +indent-guides-in-childframe-p ()
      (frame-parameter nil 'parent-frame))
    ;; indent-guides in src blocks can cause syntax highlighting to fail
    ;; abruptly for some major modes (particularly *-ts-modes or rustic-mode).
    ;; Since it's already working on the super org buffer, it's redundant to let
    ;; it work on the contents of each babel block.
    (defun +indent-guides-in-org-src-block-p ()
      (string-prefix-p " *org-src-fontification:" (buffer-name))))

  ;; HACK: The way `indent-bars-display-on-blank-lines' functions, it places
  ;;   text properties with a display property containing a newline, which
  ;;   confuses `move-to-column'. This breaks `next-line' and `evil-next-line'
  ;;   without this advice (See jdtsmith/indent-bars#22). Advising
  ;;   `line-move-to-column' isn't enough for `move-to-column' calls in various
  ;;   Evil operators (`evil-delete', `evil-change', etc).
  (defadvice! +indent-guides--prevent-passing-newline-a (fn col &rest args)
    :around #'move-to-column
    (if-let* ((indent-bars-mode)
              (indent-bars-display-on-blank-lines)
              (nlp (line-end-position))
              (dprop (get-text-property nlp 'display))
              ((seq-contains-p dprop ?\n))
              ((> col (- nlp (point)))))
        (goto-char nlp)
      (apply fn col args)))

  ;; HACK: `indent-bars-mode' interacts with some packages poorly, often
  ;;   flooding whole sections of the buffer with indent guides. This section is
  ;;   dedicated to fixing interop with those packages.
  (when (modulep! :tools magit)
    (after! magit-blame
      (add-to-list 'magit-blame-disable-modes 'indent-bars-mode)))

  (when (modulep! :tools lsp)
    ;; REVIEW: Report this upstream to `indent-bars'?
    (defadvice! +indent-guides--remove-after-lsp-ui-peek-a (&rest _)
      :after #'lsp-ui-peek--peek-new
      (when (and indent-bars-mode
                 (not indent-bars-prefer-character)
                 (overlayp lsp-ui-peek--overlay))
        (save-excursion
          (let ((indent-bars--display-function #'ignore)
                (indent-bars--display-blank-lines-function #'ignore))
            (indent-bars--fontify (overlay-start lsp-ui-peek--overlay)
                                  (1+ (overlay-end lsp-ui-peek--overlay))
                                  nil)))))
    (defadvice! +indent-guides--restore-after-lsp-ui-peek-a (&rest _)
      :after #'lsp-ui-peek--peek-hide
      (unless indent-bars-prefer-character
        (indent-bars-setup)))))
