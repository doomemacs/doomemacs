;;; ui/smooth-scroll/config.el -*- lexical-binding: t; -*-

(use-package! ultra-scroll
  :when (fboundp 'pixel-scroll-precision-mode)
  :hook (doom-first-input . ultra-scroll-mode)
  :hook (doom-first-file . ultra-scroll-mode)
  :init
  (setq scroll-conservatively 101
        scroll-margin 0)
  :config
  (add-hook 'ultra-scroll-hide-functions #'hl-todo-mode)
  (add-hook 'ultra-scroll-hide-functions #'diff-hl-flydiff-mode)
  (add-hook 'ultra-scroll-hide-functions #'jit-lock-mode)
  (add-hook 'ultra-scroll-hide-functions #'good-scroll-mode))


(use-package good-scroll
  :when (modulep! +interpolate)
  :hook (doom-first-input . good-scroll-mode)
  :config
  ;; HACK: We're using good-scroll only for interpolation; ultra-scroll is
  ;;   responsible for smoothing input scrolling (e.g. the mouse wheel), so do
  ;;   this to ensure good-scroll keeps its hands off mouse wheels/trackpads.
  (add-hook! 'good-scroll-mode-hook
    (defun +smooth-scroll-coexist-with-ultra-scroll-h ()
      (if good-scroll-mode
          (setq mwheel-scroll-up-function #'scroll-up
                mwheel-scroll-down-function #'scroll-down))))

  ;; HACK: good-scroll advises interactive motion commands to trigger
  ;;   interpolated scrolling. It expects these commands to only be called
  ;;   interactively, but there are cases (like in `ledger-mode') where they (in
  ;;   this case, `move-end-of-line') are called programmatically where the
  ;;   selected window's boundaries may be out of bounds for the target buffer.
  ;;   Cue the errors.
  ;; REVIEW: This shold be fixed upstream.
  (defadvice! +smooth-scroll--fix-out-of-bounds-error-a ()
    :override #'good-scroll--point-at-top-p
    (save-restriction
      (widen)
      (<= (line-number-at-pos (max (point) (point-min)) t)
          (1+ (line-number-at-pos (min (window-start) (point-max)) t)))))

  (defun good-scroll--convert-line-to-step (line)
    (cond ((integerp line) (* line (line-pixel-height)))
          ((or (null line) (memq '- line))
           (- (good-scroll--window-usable-height)
              (* next-screen-context-lines (line-pixel-height))))
          ((line-pixel-height))))

  (defadvice! good-scroll--scroll-up (fn &optional arg)
    :around #'scroll-up
    (if good-scroll-mode
        (good-scroll-move (good-scroll--convert-line-to-step arg))
      (funcall fn arg)))

  (defadvice! good-scroll--scroll-down (fn &optional arg)
    :around #'scroll-down
    (if good-scroll-mode
        (good-scroll-move (- (good-scroll--convert-line-to-step arg)))
      (funcall fn arg))))
