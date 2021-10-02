;;; input/bidi/config.el -*- lexical-binding: t; -*-

(defvar +bidi-mode-map (make-sparse-keymap)
  "Keymap for `+bidi-mode'.")

(defvar +bidi-hebrew-font (font-spec :family "DejaVu Sans")
  "Overriding font for hebrew script.
Must be a `font-spec', see `doom-font' for examples.

WARNING: if you specify a size for this font it will hard-lock any usage of this
font to that size. It's rarely a good idea to do so!")

(defvar +bidi-arabic-font (font-spec :family "DejaVu Sans")
  "Overriding font for arabic and arabic-derived scripts.
Must be a `font-spec', see `doom-font' for examples.

WARNING: if you specify a size for this font it will hard-lock any usage of this
font to that size. It's rarely a good idea to do so!")

;;;###autoload
(define-minor-mode +bidi-mode
  "Minor mode for using bidirectional text in a buffer.

Note that the whole buffer doesn't have to contain any
bidirectional text at all, this mode just makes bidi editing
easier."
  :keymap +bidi-mode-map
  (if +bidi-mode
      (progn
        (setq bidi-paragraph-direction nil   ; Do treat +Bidi as right-to-left
              bidi-paragraph-separate-re "^" ; No need for empty lines to switch alignment
              bidi-paragraph-start-re "^"    ; ^
              bidi-inhibit-bpa nil))         ; Better bidi paren logic
    (setq bidi-paragraph-direction 'left-to-right
          bidi-paragraph-separate-re nil
          bidi-paragraph-start-re nil
          bidi-inhibit-bpa t)))

(define-globalized-minor-mode +bidi-global-mode +bidi-mode +bidi-mode)

(add-hook! 'after-setting-font-hook
  (defun +bidi-set-fonts-h ()
    (set-fontset-font t 'hebrew +bidi-hebrew-font)
    (set-fontset-font t 'arabic +bidi-arabic-font)))
