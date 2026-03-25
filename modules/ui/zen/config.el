;;; ui/zen/config.el -*- lexical-binding: t; -*-

(defcustom +zen-mixed-pitch-modes '(adoc-mode rst-mode markdown-mode org-mode)
  "What major-modes to enable `mixed-pitch-mode' in with `writeroom-mode'."
  :type '(repeat symbol)
  :group '+zen)

(defcustom +zen-text-scale 2.0
  "The text-scaling level for `writeroom-mode'."
  :type 'float
  :group '+zen)

(defcustom +zen-window-divider-size 4
  "Pixel size of window dividers when `writeroom-mode' is active."
  :type 'integer
  :group '+zen)


;;
;;; Packages

(after! writeroom-mode
  ;; Users should be able to activate writeroom-mode in one buffer (e.g. an org
  ;; buffer) and code in another. No global behavior should be applied.
  ;; Fullscreening/maximizing will be opt-in.
  (defvar +zen--old-writeroom-global-effects writeroom-global-effects)
  (setq writeroom-global-effects nil)
  (setq writeroom-maximize-window nil)

  (add-hook! 'writeroom-local-effects :append
    (defun +zen-enable-text-scaling-mode-h (arg)
      "Enable `mixed-pitch-mode' when in `+zen-mixed-pitch-modes'."
      (when (/= +zen-text-scale 0)
        (text-scale-set (if (= arg 1) +zen-text-scale 0))
        (visual-fill-column-adjust))))

  ;; Adjust margins when text size is changed
  (advice-add #'text-scale-adjust :after #'visual-fill-column-adjust))


(use-package! mixed-pitch
  :defer t
  :init
  (add-hook! 'writeroom-local-effects
    (defun +zen-enable-mixed-pitch-mode-h (arg)
      "Enable `mixed-pitch-mode' when in `+zen-mixed-pitch-modes'."
      (if (apply #'derived-mode-p +zen-mixed-pitch-modes)
          (mixed-pitch-mode arg))))
  :config
  (dolist (face '(solaire-line-number-face
                  org-date
                  org-footnote
                  org-special-keyword
                  org-property-value
                  org-ref-cite-face
                  org-tag
                  org-todo-keyword-todo
                  org-todo-keyword-habt
                  org-todo-keyword-done
                  org-todo-keyword-wait
                  org-todo-keyword-kill
                  org-todo-keyword-outd
                  org-todo
                  org-done
                  font-lock-comment-face))
    (add-to-list 'mixed-pitch-fixed-pitch-faces face)))


(use-package! focus
  :when (modulep! +focus)
  :defer t
  :init
  (add-hook 'writeroom-local-effects #'focus-mode t))


(use-package! lsp-focus
  :when (modulep! +focus)
  :when (modulep! :tools lsp -eglot)
  :hook (focus-mode . +zen-lsp-focus-mode-h)
  :config
  (defun +zen-lsp-focus-mode-h ()
    ;; HACK: lsp-focus-mode doesn't do its own checks, throwing an error if
    ;;   lsp-mode isn't active or the given client doesn't support
    ;;   foldingRangeProvider, so we do our own checks.
    ;; REVIEW: PR a safe activator function upstream, maybe?
    (when (bound-and-true-p lsp-mode)
      (if (lsp--capability "foldingRangeProvider")
          (lsp-focus-mode +1)
        (doom-log "lsp-focus: client doesn't support foldingRangeProvider, falling back to naive boundary detection")))))
