;;; input/bidi/config.el -*- lexical-binding: t; -*-

(defvar +bidi-mode-map (make-sparse-keymap)
  "Keymap for `+bidi-mode'.")

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
