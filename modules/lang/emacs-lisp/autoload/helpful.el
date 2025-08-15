;;; lang/emacs-lisp/autoload/helpful.el -*- lexical-binding: t; -*-

(defvar +emacs-lisp--helpful-buffer-ring-size 5
  "How many buffers are stored for use with `*helpful-next'.")

(defvar +emacs-lisp--helpful-buffer-ring (make-ring +emacs-lisp--helpful-buffer-ring-size)
  "Ring that stores the current Helpful buffer history.")

(defun +emacs-lisp--helpful-buffer-index (&optional buffer)
  "If BUFFER is a Helpful buffer, return it’s index in the buffer ring."
  (let ((buf (or buffer (current-buffer))))
    (and (eq (buffer-local-value 'major-mode buf) 'helpful-mode)
         (seq-position (ring-elements +emacs-lisp--helpful-buffer-ring) buf #'eq))))

;;;###autoload
(defun +emacs-lisp-record-new-buffers-a (buf)
  "Update the buffer ring according to the current buffer and HELP-BUF."
  (let ((buf-ring +emacs-lisp--helpful-buffer-ring))
    (let ((newer-buffers (or (+emacs-lisp--helpful-buffer-index) 0)))
      (dotimes (_ newer-buffers) (ring-remove buf-ring 0)))
    (when (/= (ring-size buf-ring) +emacs-lisp--helpful-buffer-ring-size)
      (ring-resize buf-ring +emacs-lisp--helpful-buffer-ring-size))
    (ring-insert buf-ring buf)))

(defun +emacs-lisp--helpful-next (&optional buffer)
  "Return the next live Helpful buffer relative to BUFFER."
  (let ((buf-ring +emacs-lisp--helpful-buffer-ring)
        (index (or (+emacs-lisp--helpful-buffer-index buffer) -1)))
    (cl-block nil
      (while (> index 0)
        (cl-decf index)
        (let ((buf (ring-ref buf-ring index)))
          (if (buffer-live-p buf) (cl-return buf)))
        (ring-remove buf-ring index)))))

(defun +emacs-lisp--helpful-previous (&optional buffer)
  "Return the previous live Helpful buffer relative to BUFFER."
  (let ((buf-ring +emacs-lisp--helpful-buffer-ring)
        (index (1+ (or (+emacs-lisp--helpful-buffer-index buffer) -1))))
    (cl-block nil
      (while (< index (ring-length buf-ring))
        (let ((buf (ring-ref buf-ring index)))
          (if (buffer-live-p buf) (cl-return buf)))
        (ring-remove buf-ring index)))))

;;;###autoload
(defun +emacs-lisp/helpful-next ()
  "Go to the next Helpful buffer."
  (interactive)
  (if-let* ((buf (+emacs-lisp--helpful-next)))
      (funcall helpful-switch-buffer-function buf)
    (user-error "No helpful buffer to switch to")))

;;;###autoload
(defun +emacs-lisp/helpful-previous ()
  "Go to the previous Helpful buffer."
  (interactive)
  (if-let* ((buf (+emacs-lisp--helpful-previous)))
      (funcall helpful-switch-buffer-function buf)
    (user-error "No helpful buffer to switch to")))

;;; helpful.el ends here
