;;; ui/indent-guides/config.el -*- lexical-binding: t; -*-

(defcustom +indent-guides-inhibit-functions nil
  "A list of predicate functions.

Each function will be run in the context of a buffer where
`highlight-indent-guides-mode' should be enabled. If any function returns
non-nil, the mode will not be activated."
  :type 'hook
  :group '+indent-guides)


;;
;;; Packages

(use-package! highlight-indent-guides
  :hook ((prog-mode text-mode conf-mode) . +indent-guides-init-maybe-h)
  :init
  (setq highlight-indent-guides-method (if (display-graphic-p) 'bitmap 'character)
        highlight-indent-guides-bitmap-function #'highlight-indent-guides--bitmap-line)

  (defun +indent-guides-init-maybe-h ()
    "Enable `highlight-indent-guides-mode'.
Consults `+indent-guides-inhibit-functions'."
    (unless (run-hook-with-args-until-success '+indent-guides-inhibit-functions)
      (highlight-indent-guides-mode +1)))

  (add-hook! '+indent-guides-inhibit-functions
    ;; Org's virtual indentation messes up indent-guides.
    (defun +indent-guides-in-org-indent-mode-p ()
      (bound-and-true-p org-indent-mode))
    ;; Fix #6438: indent-guides prevent inline images from displaying in ein
    ;; notebooks.
    (defun +indent-guides-in-ein-notebook-p ()
      (and (bound-and-true-p ein:notebook-mode)
           (bound-and-true-p ein:output-area-inlined-images))))
  :config
  ;; HACK: If this package is loaded too early (by the user, and in terminal
  ;;   Emacs), then `highlight-indent-guides-auto-set-faces' will have been
  ;;   called much too early to set its faces correctly. To get around this, we
  ;;   need to call it again, but at a time when I can ensure a frame exists an
  ;;   the current theme is loaded.
  (when (doom-context-p 'init)
    (add-hook 'doom-first-buffer-hook #'highlight-indent-guides-auto-set-faces)))
