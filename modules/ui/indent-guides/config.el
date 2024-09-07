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
  :hook ((prog-mode text-mode conf-mode) . +indent-guides-init-maybe-h)
  :init
  (defun +indent-guides-init-maybe-h ()
    "Enable `indent-bars-mode' depending on `+indent-guides-inhibit-functions'."
    (unless (run-hook-with-args-until-success '+indent-guides-inhibit-functions)
      (indent-bars-mode +1)))
  :config
  ;; Bitmap performance is inconsistent across display systems (pgtk, ns, mac,
  ;; gtk, etc). There's also a bitmap init bug in PGTK builds of Emacs before
  ;; v30 that could cause crashes (see jdtsmith/indent-bars#3). If you use PGTK
  ;; and reverse this setting, you've been warned!
  (setq indent-bars-prefer-character t)

  ;; TODO: Uncomment once we support treesit
  ;; (setq indent-bars-treesit-support (modulep! :tools tree-sitter))

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

  ;; HACK: Out of the box, indent-bars offers no way to fully disable
  ;;   "highlighting the current line", so I advise on in, since the feature is
  ;;   unnecessary work (and allocation of timers) for users that don't want it,
  ;;   and for our performance-leaning defaults.
  (setq indent-bars-depth-update-delay nil)
  (defadvice! +indent-guides--disable-highlight-current-line-a (fn &rest args)
    :around #'indent-bars-setup
    (letf! (defun indent-bars--highlight-current-depth ()
             (when indent-bars-depth-update-delay
               (funcall indent-bars--highlight-current-depth)))
      (prog1 (apply fn args)
        (unless indent-bars-depth-update-delay
          (remove-hook 'post-command-hook #'indent-bars--highlight-current-depth t))))))
