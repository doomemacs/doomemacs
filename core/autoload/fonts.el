;;; core/autoload/fonts.el -*- lexical-binding: t; -*-

;;;###autoload
(defvar doom-font-increment 2
  "How many steps to increase the font size each time `doom/increase-font-size'
or `doom/decrease-font-size' are invoked.")

;;;###autoload
(defvar doom-big-font nil
  "The font to use for `doom-big-font-mode'. If nil, `doom-font' will be used,
scaled up by `doom-big-font-increment'. See `doom-font' for details on
acceptable values for this variable.")

;;;###autoload
(defvar doom-big-font-increment 8
  "How many steps to increase the font size (with `doom-font' as the base) when
`doom-big-font-mode' is enabled and `doom-big-font' is nil.")

;;;###autoload
(defvar doom-change-font-size-hook nil
  "A hook run after adjusting the font size with `doom/increase-font-size',
`doom/decrease-font-size', or `doom/reset-font-size'.")


;;
;;; Library

(defun doom--font-name (fontname frame)
  (when (query-fontset fontname)
    (when-let* ((ascii (assq 'ascii (aref (fontset-info fontname frame) 2))))
      (setq fontname (nth 2 ascii))))
  (or (x-decompose-font-name fontname)
      (error "Cannot decompose font name")))

;;;###autoload
(defun doom-adjust-font-size (increment &optional frame)
  "Increase size of font in FRAME by INCREMENT.
Interactively, INCREMENT is given by the prefix argument.
Optional FRAME parameter defaults to current frame."
  (interactive "p")
  (let* ((frame (or frame (selected-frame)))
         (font (frame-parameter frame 'font))
         (font (doom--font-name font frame)))
    (let ((new-size (+ (string-to-number (aref font xlfd-regexp-pixelsize-subnum))
                       increment)))
      (unless (> new-size 0)
        (error "Font is to small at %d" new-size))
      (aset font xlfd-regexp-pixelsize-subnum (number-to-string new-size)))
    ;; Set point size & width to "*", so frame width will adjust to new font size
    (aset font xlfd-regexp-pointsize-subnum "*")
    (aset font xlfd-regexp-avgwidth-subnum "*")
    (setq font (x-compose-font-name font))
    (unless (x-list-fonts font)
      (error "Cannot change font size"))
    (modify-frame-parameters frame `((font . ,font)))))


;;
;;; Commands

;;;###autoload
(defun doom/increase-font-size (count)
  "Enlargens the font size across the current frame."
  (interactive "p")
  (let ((zoom-factor (or (frame-parameter nil 'font-scale) 0))
        (increment (* count doom-font-increment)))
    (setq zoom-factor (+ zoom-factor increment))
    (if (= zoom-factor 0)
        (doom/reset-font-size)
      (doom-adjust-font-size increment)
      (modify-frame-parameters nil `((font-scale . ,zoom-factor)))
      (run-hooks 'doom-change-font-size-hook))))

;;;###autoload
(defun doom/decrease-font-size (count)
  "Shrinks the font size across the current frame."
  (interactive "p")
  (doom/increase-font-size (- count)))

;;;###autoload
(defun doom/reset-font-size ()
  "Reset font size.

Assuming it has been adjusted via `doom/increase-font-size' and
`doom/decrease-font-size'."
  (interactive)
  (let ((zoom-factor (frame-parameter nil 'font-scale)))
    (if (not zoom-factor)
        (user-error "Font size hasn't been changed")
      (set-frame-font doom-font t)
      (modify-frame-parameters nil '((font-scale)))
      (run-hooks 'doom-change-font-size-hook))))

;;;###autoload
(define-minor-mode doom-big-font-mode
  "A global mode that resizes the font, for streams, screen-sharing and
presentations.

This uses `doom/increase-font-size' under the hood, and enlargens the font by
`doom-big-font-increment'."
  :init-value nil
  :lighter " BIG"
  :global t
  (unless doom-font
    (user-error "`doom-font' must be set to a valid font"))
  (let ((frame (selected-frame)))
    (if doom-big-font
        (progn
          (set-frame-font (if doom-big-font-mode doom-big-font doom-font)
                          t (list frame))
          (run-hooks 'doom-change-font-size-hook))
      (set-frame-font doom-font t (list frame))
      (when doom-big-font-mode
        (doom-adjust-font-size doom-big-font-increment frame)))))
