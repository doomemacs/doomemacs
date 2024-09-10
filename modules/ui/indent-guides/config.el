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
  (setq indent-bars-prefer-character
        (or
         ;; Bitmaps are far slower on MacOS, inexplicably, but this needs more
         ;; testing to see if it's specific to ns or emacs-mac builds, or is
         ;; just a general MacOS issue.
         (featurep :system 'macos)
         ;; FIX: A bitmap init bug in PGTK builds of Emacs before v30 that could
         ;; cause crashes (see jdtsmith/indent-bars#3).
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

  ;; TODO: Uncomment once we support treesit
  ;; (setq indent-bars-treesit-support (modulep! :tools tree-sitter))

  ;; HACK: Both indent-bars and tree-sitter-hl-mode use the jit-font-lock
  ;;   mechanism, and so they don't play well together. For those particular
  ;;   cases, we'll use `highlight-indent-guides', at least until the
  ;;   tree-sitter module adopts treesit.
  (defvar-local +indent-guides-p nil)
  (add-hook! 'tree-sitter-mode-hook :append
    (defun +indent-guides--toggle-on-tree-sitter-h ()
      (if tree-sitter-mode
          (when (bound-and-true-p indent-bars-mode)
            (with-memoization (get 'indent-bars-mode 'disabled-in-tree-sitter)
              (doom-log "Disabled `indent-bars-mode' because it's not supported in `tree-sitter-mode'")
              t)
            (indent-bars-mode -1)
            (setq +indent-guides-p t))
        (when +indent-guides-p
          (indent-bars-mode +1)))))

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
    ;; Don't display indent guides in childframe popups (not helpful in
    ;; completion or eldoc popups).
    ;; REVIEW: Swap with `frame-parent' when 27 support is dropped
    (defun +indent-guides-in-childframe-p ()
      (frame-parameter nil 'parent-frame)))

  ;; HACK: `indent-bars-mode' interactions with some packages poorly. This
  ;;   section is dedicated to package interop fixes.
  (when (modulep! :tools magit)
    (after! magit-blame
      (add-to-list 'magit-blame-disable-modes 'indent-bars-mode)))

  (when (modulep! :tools lsp)
    ;; HACK: lsp-ui-peek uses overlays, and indent-bars doesn't know how to deal
    ;;   with all the whitespace it uses to format its popups, spamming it with
    ;;   indent guides. Making the two work together is a project for another
    ;;   day, so disable `indent-bars-mode' while its active instead. Doesn't
    ;;   affect character bars though.
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
