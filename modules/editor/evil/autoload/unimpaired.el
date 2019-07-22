;;; editor/evil/autoload/unimpaired.el -*- lexical-binding: t; -*-

;; These are ported from vim-unimpaired https://github.com/tpope/vim-unimpaired
;; and bound in the :config default module (in +evil-bindings.el).

;;
;;; Next/Previous commands

;;; ] SPC / [ SPC
;;;###autoload
(defun +evil/insert-newline-below (count)
  "Insert COUNT blank line(s) below current line. Does not change modes."
  (interactive "p")
  (dotimes (_ count)
    (save-excursion (evil-insert-newline-below))))

;;;###autoload
(defun +evil/insert-newline-above (count)
  "Insert COUNT blank line(s) above current line. Does not change modes."
  (interactive "p")
  (dotimes (_ count)
    (save-excursion (evil-insert-newline-above))))

;;; ]t / [t
;;;###autoload
(defun +evil/next-frame (count)
  "Focus next frame."
  (interactive "p")
  (dotimes (_ (abs count))
    (let ((frame (if (> count 0) (next-frame) (previous-frame))))
      (if (eq frame (selected-frame))
          (user-error "No other frame")
        (select-frame-set-input-focus frame)))))

;;;###autoload
(defun +evil/previous-frame (count)
  "Focus previous frame."
  (interactive "p")
  (+evil/next-frame (- count)))

;;; ]f / [f
(defun +evil--next-file (n)
  (unless buffer-file-name
    (user-error "Must be called from a file-visiting buffer"))
  (let* ((directory (file-name-directory buffer-file-name))
         (filename (file-name-nondirectory buffer-file-name))
         (files (doom-glob (file-name-directory buffer-file-name) "[!.]*"))
         (index (cl-position filename files :test #'file-equal-p)))
    (when (null index)
      (user-error "Couldn't find this file in current directory"))
    (let ((index (+ index n)))
      (cond ((>= index (length files))
             (user-error "No files after this one"))
            ((< index 0)
             (user-error "No files before this one"))
            ((expand-file-name (nth index files) directory))))))

;;;###autoload
(defun +evil/next-file (count)
  "Open file following this one, alphabetically, in the same directory."
  (interactive "p")
  (find-file (+evil--next-file count)))

;;;###autoload
(defun +evil/previous-file (count)
  "Open file preceding this one, alphabetically, in the same directory."
  (interactive "p")
  (find-file (+evil--next-file (- count))))


;;
;;; Encoding/Decoding

;; NOTE For ]x / [x see :lang web
;; - `+web:encode-html-entities'
;; - `+web:decode-html-entities'

(defun +evil--encode (beg end fn)
  (save-excursion
    (goto-char beg)
    (let* ((end (if (eq evil-this-type 'line) (1- end) end))
           (text (buffer-substring-no-properties beg end)))
      (delete-region beg end)
      (insert (funcall fn text)))))

;;; ]u / [u
;;;###autoload (autoload '+evil:url-encode "editor/evil/autoload/unimpaired" nil t)
(evil-define-operator +evil:url-encode (count &optional beg end type)
  "TODO"
  (interactive "<c><R>")
  (+evil--encode beg end #'url-encode-url))

;;;###autoload (autoload '+evil:url-decode "editor/evil/autoload/unimpaired" nil t)
(evil-define-operator +evil:url-decode (count &optional beg end type)
  "TODO"
  (interactive "<c><R>")
  (+evil--encode beg end #'url-unhex-string))

;;; ]y / [y
;;;###autoload (autoload '+evil:c-string-encode "editor/evil/autoload/unimpaired" nil t)
(evil-define-operator +evil:c-string-encode (count &optional beg end type)
  "TODO"
  (interactive "<c><R>")
  (+evil--encode
   beg end
   (lambda (text)
     (replace-regexp-in-string "[\"\\]" (lambda (ch) (concat "\\" ch)) text))))

;;;###autoload (autoload '+evil:c-string-decode "editor/evil/autoload/unimpaired" nil t)
(evil-define-operator +evil:c-string-decode (count &optional beg end type)
  "TODO"
  (interactive "<c><R>")
  (+evil--encode
   beg end
   (lambda (text)
     (replace-regexp-in-string "\\\\[\"\\]" (lambda (str) (substring str 1)) text))))


;;
;;; Standalone

;;; gp
;;;###autoload
(defun +evil/reselect-paste ()
  "Return to visual mode and reselect the last pasted region."
  (interactive)
  (cl-destructuring-bind (_ _ _ beg end &optional _)
      evil-last-paste
    (evil-visual-make-selection
     (save-excursion (goto-char beg) (point-marker))
     end)))
