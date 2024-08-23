;;; input/bidi/config.el -*- lexical-binding: t; -*-

(defvar +bidi-hebrew-font (font-spec :family "DejaVu Sans")
  "Overriding font for hebrew script.
Must be a `font-spec', see `doom-font' for examples.

WARNING: if you specify a size for this font it will hard-lock any usage of this
font to that size. It's rarely a good idea to do so! Set
`+bidi-hebrew-font-scale' to scale the font up or down.")

(defcustom +bidi-hebrew-font-scale 1.0
  "What scale to display `+bidi-hebrew-font' at."
  :type 'float)

(defface +bidi-hebrew-face `((t :font ,+bidi-hebrew-font)) "")

(defvar +bidi-arabic-font (font-spec :family "DejaVu Sans")
  "Overriding font for arabic and arabic-derived scripts.
Must be a `font-spec', see `doom-font' for examples.

WARNING: if you specify a size for this font it will hard-lock any usage of this
font to that size. It's rarely a good idea to do so! Set
`+bidi-arabic-font-scale' to scale the font up or down.")

(defcustom +bidi-arabic-font-scale 1.0
  "What scale to display `+bidi-arabic-font' at."
  :type 'float)

(defface +bidi-arabic-face `((t :font ,+bidi-arabic-font)) "")

(defcustom +bidi-want-smart-fontify t
  "Use bidi override fonts on surrounding space and punctuation as well.
Add `+bidi-smart-fontify-keywords' to `font-lock-keywords' on editable buffers
when `+bidi-mode' is on."
  :type 'boolean)

(defvar +bidi-smart-fontify-keywords
  `((,(rx (any (#x0590 . #x05FF))       ; Hebrew
          (group (one-or-more (any " " punctuation))))
     (1 '+bidi-hebrew-face t))
    (,(rx (or (any (#x0600 . #x06FF))   ; Arabic
              (any (#x0750 . #x077F))   ; Arabic Supplement
              (any (#x0870 . #x089F))   ; Arabic Extended-B
              (any (#x08A0 . #x08FF)))  ; Arabic Extended-A
          (group (one-or-more (any " " punctuation))))
     (1 '+bidi-arabic-face t)))

  "`font-lock' keywords matching spaces and punctuation after RTL characters.
See the variable `font-lock-keywords' for information on the format.")

(defcustom +bidi-paragraph-direction nil
  "The value of `bidi-paragragh-direction' when `+bidi-mode' is on.
See the `bidi-paragraph-direction' for more info.

Warning: do not change this if you are using `+bidi-global-mode'.'"
  :type '(choice
          (const :tag "Left to Right" left-to-right)
          (const :tag "Right to Left" right-to-left)
          (const :tag "Dynamic, according to paragraph text" nil)))


(defvar +bidi-mode-map (make-sparse-keymap)
  "Keymap for `+bidi-mode'.")

(define-minor-mode +bidi-mode
  "Minor mode for using bidirectional text in a buffer.

Note that the whole buffer doesn't have to contain any
bidirectional text at all, this mode just makes bidi editing
easier."
  :keymap +bidi-mode-map
  (if +bidi-mode
      (progn
        (setq bidi-paragraph-direction +bidi-paragraph-direction   ; Better paragraph alignment
              bidi-paragraph-separate-re "^" ; No need for empty lines to switch alignment
              bidi-paragraph-start-re "^"    ; ^
              bidi-inhibit-bpa nil)          ; Better bidi paren logic
        (when (and +bidi-want-smart-fontify
                   (not buffer-read-only))
          (font-lock-add-keywords
           nil
           +bidi-smart-fontify-keywords
           'append)
          (font-lock-flush)))
    (setq bidi-paragraph-direction 'left-to-right
          bidi-paragraph-separate-re nil
          bidi-paragraph-start-re nil
          bidi-inhibit-bpa t)
    (when (and +bidi-want-smart-fontify
               (not buffer-read-only))
      (font-lock-remove-keywords
       nil
       +bidi-smart-fontify-keywords)
      (font-lock-flush))))

(define-globalized-minor-mode +bidi-global-mode +bidi-mode +bidi-mode)


(defun +bidi--set-font (name)
  (when-let* ((font-name (format "+bidi-%s-font" name))
              (font-var (or (intern-soft font-name) (error "Invalid font: %s" name)))
              (font (symbol-value font-var)))
    (condition-case e
        (let ((scale (symbol-value (intern (format "+bidi-%s-font-scale" name)))))
          (set-fontset-font t 'hebrew font)
          (set-face-font (intern (format "+bidi-%s-face" name)) font)
          (when (/= scale 1.0)
            (setf (alist-get (font-get font :family) face-font-rescale-alist nil nil #'equal)
                  scale)))
      ('error
       (if (string-prefix-p "Font not available" (error-message-string e))
           (warn "Missing font for `%s': %s" font-name (font-get font :family))
         (signal (car e) (cdr e)))))))

(add-hook! 'after-setting-font-hook
  (defun +bidi-init-fonts-h ()
    (+bidi--set-font 'hebrew)
    (+bidi--set-font 'arabic)))
