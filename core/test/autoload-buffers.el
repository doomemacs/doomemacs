;; -*- no-byte-compile: t; -*-
;;; core/test/autoload-buffers.el

(defmacro with-temp-buffers!! (buffer-args &rest body)
  (declare (indent defun))
  (let (buffers)
    (dolist (bsym buffer-args)
      (push `(,bsym (get-buffer-create ,(symbol-name bsym)))
            buffers))
    `(cl-flet ((buffer-list
                (lambda ()
                  (cl-remove-if-not #'buffer-live-p (list ,@(reverse (mapcar #'car buffers)))))))
       (let* ((split-width-threshold 0)
              (window-min-width 0)
              persp-mode
              ,@buffers)
         (delete-other-windows)
         ,@body
         (let (kill-buffer-query-functions kill-buffer-hook)
           (mapc #'kill-buffer (buffer-list)))))))

;;
(def-test! get-buffers
  (with-temp-buffers!! (a b c)
    (should (cl-every #'buffer-live-p (buffer-list)))
    (should (equal (buffer-list) (list a b c)))
    (dolist (buf (list (cons a doom-emacs-dir)
                       (cons b doom-emacs-dir)
                       (cons c "/tmp/")))
      (with-current-buffer (car buf)
        (setq-local default-directory (cdr buf))))
    (projectile-mode +1)
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
        (should (cl-every (lambda (x) (memq x buffers)) (list a b c)))))
    (projectile-mode -1)))

(def-test! real-buffers
  (let (doom-real-buffer-functions)
    (with-temp-buffers!! (a b c d)
      (with-current-buffer a
        (setq-local buffer-file-name "x"))
      (with-current-buffer b
        (setq-local doom-real-buffer-p t))
      (with-current-buffer c
        (rename-buffer "*C*"))
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
  (with-temp-buffers!! (a b c d)
    (switch-to-buffer a)
    (should (eq (current-buffer) a))
    (should (eq (selected-window) (get-buffer-window a)))
    (split-window nil 1)
    (switch-to-buffer b)
    (should (eq (current-buffer) b))
    (should (eq (selected-window) (get-buffer-window b)))
    (should (cl-intersection (list a b) (doom-visible-buffers)))
    (should (cl-intersection (list c d) (doom-buried-buffers)))
    (should (cl-intersection (mapcar #'get-buffer-window (list a b))
                             (doom-visible-windows)))))

;; `doom-matching-buffers'
(def-test! matching-buffers
  (with-temp-buffers!! (a b c)
    (let ((buffers (doom-matching-buffers "^[ac]$")))
      (should (= 2 (length buffers)))
      (should (cl-every #'bufferp buffers))
      (should (cl-every (lambda (x) (memq x buffers)) (list a c)))
      (should (equal buffers (doom-matching-buffers "^[ac]$"))))))

;; `doom-buffers-in-mode'
(def-test! buffers-in-mode
  (with-temp-buffers!! (a b c d e)
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

;; `doom-fallback-buffer'
(def-test! fallback-buffer
  (let ((fallback (doom-fallback-buffer)))
    (should (buffer-live-p fallback))
    (should (equal (buffer-name fallback) doom-fallback-buffer-name))))

;; `doom-kill-buffer-and-windows'
(def-test! kill-buffer-and-windows
  (with-temp-buffers!! (a b)
    (switch-to-buffer a) (split-window-horizontally)
    (switch-to-buffer b) (split-window-horizontally)
    (switch-to-buffer a)

    (should (= (length (doom-visible-windows)) 3))
    (should (= (length (doom-buffer-list)) 2))

    (doom-kill-buffer-and-windows a)
    (should-not (buffer-live-p a))
    (should (= (length (doom-visible-windows)) 1))))

;; TODO doom/kill-all-buffers
;; TODO doom/kill-other-buffers
;; TODO doom/kill-matching-buffers
;; TODO doom/cleanup-session
