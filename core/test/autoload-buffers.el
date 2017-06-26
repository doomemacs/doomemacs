;; -*- no-byte-compile: t; -*-
;;; core/test/autoload-buffers.el

(defmacro -with-temp-buffers! (buffer-args &rest body)
  (declare (indent defun))
  (let (buffers)
    (dolist (bsym buffer-args)
      (push `(,bsym (get-buffer-create ,(symbol-name bsym)))
            buffers))
    `(save-window-excursion
       (cl-flet ((buffer-list (lambda () (list ,@(reverse (mapcar #'car buffers))))))
         (let* (persp-mode
                ,@buffers)
           ,@body
           (mapc #'kill-buffer (buffer-list)))))))

;;
(def-test! get-buffers
  (-with-temp-buffers! (a b c)
    (should (cl-every #'buffer-live-p (buffer-list)))
    (should (equal (buffer-list) (list a b c)))
    (dolist (buf (list (cons a doom-emacs-dir)
                       (cons b doom-emacs-dir)
                       (cons c "/tmp/")))
      (with-current-buffer (car buf)
        (setq-local default-directory (cdr buf))))
    (with-current-buffer a
      ;; should produce all buffers
      (let ((buffers (doom-buffer-list)))
        (should (cl-every (lambda (x) (memq x buffers)) (list a b c))))
      ;; should produce only project buffers
      (let ((buffers (doom-project-buffer-list)))
        (should (cl-every (lambda (x) (memq x buffers)) (list a b)))
        (should-not (memq c buffers))))
    ;; If no project is available, just get all buffers
    (with-current-buffer c
      (let ((buffers (doom-project-buffer-list)))
        (should (cl-every (lambda (x) (memq x buffers)) (list a b c)))))))

(def-test! real-buffers
  (let (doom-real-buffer-functions)
    (-with-temp-buffers! (a b c d)
      (dolist (buf (list a b))
        (with-current-buffer buf
          (setq-local buffer-file-name "x")))
      (with-current-buffer c
        (rename-buffer "*C*"))
      (with-current-buffer d
        (doom-popup-mode +1))
      (should (doom-real-buffer-p a))
      (should (doom-real-buffer-p b))
      (should-not (doom-real-buffer-p c))
      (should-not (doom-real-buffer-p d))
      (let ((buffers (doom-real-buffer-list)))
        (should (= (length buffers) 2))
        (should (cl-every  (lambda (x) (memq x buffers)) (list a b)))
        (should (cl-notany (lambda (x) (memq x buffers)) (list c d)))))))

;; `doom-visible-windows'
;; `doom-visible-buffers'
;; `doom-buried-buffers'
(def-test! visible-buffers-and-windows
  (-with-temp-buffers! (a b c d)
    (switch-to-buffer a)
    (should (eq (current-buffer) a))
    (should (eq (selected-window) (get-buffer-window a)))
    (split-window)
    (switch-to-buffer b)
    (should (eq (current-buffer) b))
    (should (eq (selected-window) (get-buffer-window b)))
    (should (cl-intersection (list a b) (doom-visible-buffers)))
    (should (cl-intersection (list c d) (doom-buried-buffers)))
    (should (cl-intersection (mapcar #'get-buffer-window (list a b))
                             (doom-visible-windows)))))

;; `doom-matching-buffers'
(def-test! matching-buffers
  (-with-temp-buffers! (a b c)
    (let ((buffers (doom-matching-buffers "^[ac]$")))
      (should (= 2 (length buffers)))
      (should (cl-every #'bufferp buffers))
      (should (cl-every (lambda (x) (memq x buffers)) (list a c)))
      (should (equal buffers (doom-matching-buffers "^[ac]$"))))))

;; `doom-buffers-in-mode'
(def-test! buffers-in-mode
  (-with-temp-buffers! (a b c d e)
    (dolist (buf (list a b))
      (with-current-buffer buf
        (emacs-lisp-mode)))
    (dolist (buf (list c d e))
      (with-current-buffer buf
        (text-mode)))
    (let ((el-buffers  (doom-buffers-in-mode 'emacs-lisp-mode))
          (txt-buffers (doom-buffers-in-mode 'text-mode)))
      (should (cl-every #'buffer-live-p (append el-buffers txt-buffers)))
      (should (= 2 (length el-buffers)))
      (should (= 3 (length txt-buffers))))))

;; `doom-kill-buffer'
(def-test! kill-buffer
  (-with-temp-buffers! (a b)
    (doom-kill-buffer a)
    (should-not (buffer-live-p a))
    ;; modified buffer
    (with-current-buffer b
      (set-buffer-modified-p t))
    (doom-kill-buffer b t)
    (should-not (buffer-live-p a))))

;; `doom--cycle-real-buffers'
(def-test! kill-buffer-then-show-real-buffer
  (-with-temp-buffers! (a b c d)
    (dolist (buf (list a b d))
      (with-current-buffer buf
        (setq-local buffer-file-name "x")))
    (should (cl-every #'buffer-live-p (buffer-list)))
    (switch-to-buffer a)
    (should (eq (current-buffer) a))
    (should (eq (selected-window) (get-buffer-window a)))
    (should (doom-kill-buffer a))
    ;; eventually end up in the fallback buffer
    (let ((fallback (doom-fallback-buffer)))
      (while (not (eq (current-buffer) fallback))
        (should (doom-real-buffer-p))
        (doom-kill-buffer))
      (should (eq (current-buffer) fallback)))))

;; TODO doom/kill-all-buffers
;; TODO doom/kill-other-buffers
;; TODO doom/kill-matching-buffers
;; TODO doom/cleanup-buffers
