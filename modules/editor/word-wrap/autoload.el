;;; editor/word-wrap/autoload.el -*- lexical-binding: t; -*-

(defvar +word-wrap--prev-adaptive-wrap-mode nil)
(defvar +word-wrap--prev-visual-line-mode nil)
(defvar +word-wrap--major-mode-indent-var nil)

(defun +word-wrap--adjust-extra-indent-a (orig-fn beg end)
  "Contextually adjust extra word-wrap indentation."
  (let ((adaptive-wrap-extra-indent (+word-wrap--calc-extra-indent beg)))
    (funcall orig-fn beg end)))

(defun +word-wrap--calc-extra-indent (p)
  "Calculate extra word-wrap indentation at point."
  (if (not (sp-point-in-comment p))
      (pcase +word-wrap-extra-indent
        ('double
         (* 2 (symbol-value +word-wrap--major-mode-indent-var)))
        ('single
         (symbol-value +word-wrap--major-mode-indent-var))
        ((and (pred integerp) fixed)
         fixed)
        (_ 0))
    0))

;;;###autoload
(define-minor-mode +word-wrap-mode
  "Wrap long lines in the buffer with language-aware indentation.

This mode configures `adaptive-wrap' and `visual-line-mode' to wrap long lines
without modifying the buffer content. This is useful when dealing with legacy
code which contains gratuitously long lines, or running emacs on your
wrist-phone.

Wrapped lines will be indented to match the preceding line. Lines which are not
inside a comment will have additional indentation according to the configuration
of `+word-wrap-extra-indent'."
  :init-value nil
  (if +word-wrap-mode
      (progn
        (require 'adaptive-wrap)
        (require 'dtrt-indent) ; for dtrt-indent--search-hook-mapping
        (require 'smartparens) ; for sp-point-in-string-or-comment

        (setq-local +word-wrap--prev-adaptive-wrap-mode adaptive-wrap-prefix-mode)
        (setq-local +word-wrap--prev-visual-line-mode visual-line-mode)
        (setq-local +word-wrap--major-mode-indent-var
                    (caddr (dtrt-indent--search-hook-mapping major-mode)))

        (advice-add #'adaptive-wrap-fill-context-prefix :around #'+word-wrap--adjust-extra-indent-a)

        (unless +word-wrap--prev-adaptive-wrap-mode
          (adaptive-wrap-prefix-mode +1))
        (unless +word-wrap--prev-visual-line-mode
          (visual-line-mode +1)))

    ;; disable +word-wrap-mode
    (advice-remove #'adaptive-wrap-fill-context-prefix #'+word-wrap--adjust-extra-indent-a)

    (unless +word-wrap--prev-adaptive-wrap-mode
      (adaptive-wrap-prefix-mode -1))
    (unless +word-wrap--prev-visual-line-mode
      (visual-line-mode -1))))

(defun +word-wrap--enable-global-mode ()
  "Enable `+word-wrap-mode' for `+word-wrap-global-mode'.

Wrapping will be automatically enabled in all modes except special modes, or
modes explicitly listed in `+word-wrap-disabled-modes'."
  (unless (or (eq (get major-mode 'mode-class) 'special)
              (memq major-mode +word-wrap-disabled-modes))
    (+word-wrap-mode +1)))

;;;###autoload
(define-globalized-minor-mode +global-word-wrap-mode
  +word-wrap-mode
  +word-wrap--enable-global-mode)
