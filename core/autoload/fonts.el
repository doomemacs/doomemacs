;;; core/autoload/fonts.el -*- lexical-binding: t; -*-

;;;###autoload
(defvar doom-font-increment 2
  "How many steps to increase the font size each time `doom/increase-font-size'
or `doom/decrease-font-size' are invoked.")

;;;###autoload
(defvar doom-big-font nil
  "The font to use for `doom-big-font-mode'.
If nil, `doom-font' will be used, scaled up by `doom-big-font-increment'. See
`doom-font' for details on acceptable values for this variable.")

;;;###autoload
(defvar doom-big-font-increment 4
  "How many steps to increase the font size (with `doom-font' as the base) when
`doom-big-font-mode' is enabled and `doom-big-font' is nil.")


;;
;;; Library

(defun doom--font2xlfd (font)
  (cond ((stringp font) (aref (font-info font) 0))
        ((fontp font)   (font-xlfd-name font))
        ((vectorp font) font)))

;;;###autoload
(defun doom-adjust-font-size (increment &optional fixed-size-p font-alist)
  "Increase size of font in FRAME by INCREMENT.

If FIXED-SIZE-P is non-nil, treat INCREMENT as a font size, rather than a
scaling factor.

FONT-ALIST is an alist give temporary values to certain Doom font variables,
like `doom-font' or `doom-variable-pitch-font'. e.g.

  `((doom-font . ,(font-spec :family \"Sans Serif\" :size 12)))

Doesn't work in terminal Emacs."
  (unless (display-multi-font-p)
    (user-error "Cannot resize fonts in terminal Emacs"))
  (condition-case-unless-debug e
      (let (changed)
        (dolist (sym '((doom-font . default)
                       (doom-serif-font . fixed-pitch-serif)
                       (doom-variable-pitch-font . variable-pitch))
                     (when changed
                       (doom-init-fonts-h 'reload)
                       t))
          (cl-destructuring-bind (var . face) sym
            (if (null increment)
                (when (get var 'initial-value)
                  (set var (get var 'initial-value))
                  (put var 'new-size nil)
                  (put var 'initial-value nil)
                  (setq changed t))
              (let* ((orig-font (or (symbol-value var)
                                    (face-font face t)
                                    (with-temp-buffer (face-font face))))
                     (font (doom--font2xlfd orig-font))
                     (font (or (and (query-fontset font)
                                    (if-let (ascii (assq 'ascii (aref (fontset-info font) 2)))
                                        (nth 2 ascii)
                                      font))
                               font))
                     (dfont (and (stringp font) (x-decompose-font-name font)))
                     (dfont (if-let* ((remap-font (alist-get var font-alist))
                                      (remap-xlfd (if (stringp remap-font)
                                                      (aref (font-info remap-font) 0)
                                                    (font-xlfd-name remap-font))))
                                (x-decompose-font-name remap-xlfd)
                              dfont)))
                (unless (get var 'initial-value)
                  (put var 'initial-value orig-font))
                (unless (vectorp dfont)
                  (error "Could not decompose %S font: %S" var font))
                (let* ((step      (if fixed-size-p 0 (* increment doom-font-increment)))
                       (orig-size (string-to-number (aref dfont xlfd-regexp-pixelsize-subnum)))
                       (new-size  (if fixed-size-p increment (+ orig-size step))))
                  (unless (> new-size 0)
                    (error "`%s' font is too small to be reszied (%d)" var new-size))
                  (if (= orig-size new-size)
                      (message "Could not resize `%s' for some reason" var)
                    (put var 'new-size new-size)
                    (aset dfont xlfd-regexp-pixelsize-subnum (number-to-string new-size))
                    ;; Set point size & width to "*", so frame width will adjust to new font size
                    (aset dfont xlfd-regexp-pointsize-subnum "*")
                    (aset dfont xlfd-regexp-avgwidth-subnum "*")
                    (setq font (x-compose-font-name dfont))
                    (unless (x-list-fonts font)
                      (error "Cannot change font size"))
                    (set var font)
                    (setq changed t))))))))
    (error
     (ignore-errors (doom-adjust-font-size nil))
     (signal (car e) (cdr e)))))


;;
;;; Commands

;;;###autoload
(defun doom/reload-font ()
  "Reload your fonts, if they're set.
See `doom-init-fonts-h'."
  (interactive)
  (doom-init-fonts-h 'reload))

;;;###autoload
(defun doom/increase-font-size (count &optional increment)
  "Enlargens the font size across the current and child frames."
  (interactive "p")
  (doom-adjust-font-size (* count (or increment doom-font-increment))))

;;;###autoload
(defun doom/decrease-font-size (count &optional increment)
  "Shrinks the font size across the current and child frames."
  (interactive "p")
  (doom-adjust-font-size (* (- count) (or increment doom-font-increment))))

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
    (cond (doom-big-font-mode
           (message "Disabling `doom-big-font-mode'")
           (doom-big-font-mode -1)
           (setq success t))
          ((doom-adjust-font-size nil)
           (setq success t)))
    (unless success
      (user-error "The font hasn't been resized"))))

;;;###autoload
(define-minor-mode doom-big-font-mode
  "Globally resizes your fonts for streams, screen-sharing or presentations.

Uses `doom-big-font' if its set, otherwise uses `doom-font' (falling back to
your system font).

Also resizees `doom-variable-pitch-font' and `doom-serif-font'."
  :init-value nil
  :lighter " BIG"
  :global t
  (if doom-big-font
      ;; Use `doom-big-font' in lieu of `doom-font'
      (doom-adjust-font-size
       (and doom-big-font-mode
            (aref (x-decompose-font-name (doom--font2xlfd font))
                  xlfd-regexp-pixelsize-subnum))
       t `((doom-font . ,doom-big-font)))
    ;; Resize the current font
    (doom-adjust-font-size (if doom-big-font-mode doom-big-font-increment))))
