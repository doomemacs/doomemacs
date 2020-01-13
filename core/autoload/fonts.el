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


;;
;;; Library

(defun doom--font-name (fontname)
  (when (query-fontset fontname)
    (when-let (ascii (assq 'ascii (aref (fontset-info fontname) 2)))
      (setq fontname (nth 2 ascii))))
  (or (x-decompose-font-name fontname)
      (error "Cannot decompose font name")))

(defvar doom--font-scale nil)
;;;###autoload
(defun doom-adjust-font-size (increment)
  "Increase size of font in FRAME by INCREMENT.
FRAME parameter defaults to current frame."
  (if (null increment)
      (progn
        (set-frame-font doom-font 'keep-size t)
        (setf (alist-get 'font default-frame-alist)
              (cond ((stringp doom-font) doom-font)
                    ((fontp doom-font) (font-xlfd-name doom-font))
                    ((signal 'wrong-type-argument (list '(fontp stringp)
                                                        doom-font)))))
        t)
    (let* ((font (frame-parameter nil 'font))
           (font (doom--font-name font))
           (increment (* increment doom-font-increment))
           (zoom-factor (or doom--font-scale 0))
           success)
      (let ((new-size (+ (string-to-number (aref font xlfd-regexp-pixelsize-subnum))
                         increment)))
        (unless (> new-size 0)
          (error "Font is too small at %d" new-size))
        (aset font xlfd-regexp-pixelsize-subnum (number-to-string new-size)))
      ;; Set point size & width to "*", so frame width will adjust to new font size
      (aset font xlfd-regexp-pointsize-subnum "*")
      (aset font xlfd-regexp-avgwidth-subnum "*")
      (setq font (x-compose-font-name font))
      (unless (x-list-fonts font)
        (error "Cannot change font size"))
      (set-frame-font font 'keep-size t)
      (setf (alist-get 'font default-frame-alist) font)
      (setq doom--font-scale (+ zoom-factor increment))
      ;; Unlike `set-frame-font', `set-frame-parameter' won't trigger this
      (run-hooks 'after-setting-font-hook))))


;;
;;; Commands

;;;###autoload
(defun doom/reload-font ()
  "Reload your fonts, if they're set.
See `doom-init-fonts-h'."
  (interactive)
  (when doom-font
    (set-frame-font doom-font t))
  (doom-init-fonts-h)
  (mapc #'doom-init-extra-fonts-h (frame-list)))

;;;###autoload
(defun doom/increase-font-size (count)
  "Enlargens the font size across the current and child frames."
  (interactive "p")
  (doom-adjust-font-size count))

;;;###autoload
(defun doom/decrease-font-size (count)
  "Shrinks the font size across the current and child frames."
  (interactive "p")
  (doom-adjust-font-size (- count)))

;;;###autoload
(defun doom/reset-font-size ()
  "Reset font size and `text-scale'.

Assuming it has been adjusted via `doom/increase-font-size' and
`doom/decrease-font-size', or `text-scale-*' commands."
  (interactive)
  (let (success)
    (when (and (boundp 'text-scale-mode-amount)
               (/= text-scale-mode-amount 0))
      (text-scale-set 0)
      (setq success t))
    (when (doom-adjust-font-size nil)
      (setq success t))
    (unless success
      (user-error "The font hasn't been resized"))))

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
  (if doom-big-font
      (let ((font (if doom-big-font-mode doom-big-font doom-font)))
        (set-frame-font font 'keep-size t)
        (setf (alist-get 'font default-frame-alist)
              (cond ((stringp doom-font) font)
                    ((fontp font) (font-xlfd-name font))
                    ((signal 'wrong-type-argument (list '(fontp stringp)
                                                        font))))))
    (doom-adjust-font-size
     (and doom-big-font-mode
          (integerp doom-big-font-increment)
          (/= doom-big-font-increment 0)
          doom-big-font-increment))))
