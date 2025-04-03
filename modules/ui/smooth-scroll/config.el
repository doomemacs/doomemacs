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
  (add-hook 'ultra-scroll-hide-functions #'jit-lock-mode))


(use-package good-scroll
  :when (modulep! +interpolate)
  :hook (doom-first-input . good-scroll-mode)
  :config
  (defun good-scroll--convert-line-to-step (line)
    (cond ((integerp line) (* line (line-pixel-height)))
          ((or (null line) (memq '- line))
           (- (good-scroll--window-usable-height)
              (* next-screen-context-lines (line-pixel-height))))
          ((line-pixel-height))))

  (defadvice! good-scroll--scroll-up (&optional arg)
    :override #'scroll-up
    (good-scroll-move (good-scroll--convert-line-to-step arg)))

  (defadvice! good-scroll--scroll-down (&optional arg)
    :override #'scroll-down
    (good-scroll-move (- (good-scroll--convert-line-to-step arg)))))
