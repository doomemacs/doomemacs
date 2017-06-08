;;; test/core/autoload/test-buffers.el

(defmacro with-temp-buffers! (buffer-args &rest body)
  (declare (indent defun))
  (let (buffers)
    (dolist (bsym buffer-args)
      (push `(,bsym (get-buffer-create ,(symbol-name bsym)))
            buffers))
    `(let* (,@buffers
            (buffer-list (list ,@(reverse (mapcar #'car buffers)))))
       ,@body
       (mapc #'kill-buffer buffer-list))))

(def-test-group! core/autoload/buffers
  (ert-deftest get-buffers ()
    (with-temp-buffers! (a b c)
      (should (cl-every #'buffer-live-p buffer-list))
      (should (equal buffer-list (list a b c)))
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
        (let ((buffers (doom-buffer-list t)))
          (should (cl-every (lambda (x) (memq x buffers)) (list a b)))
          (should-not (memq c buffers))))
      ;; If no project is available, just get all buffers
      (with-current-buffer c
        (let ((buffers (doom-buffer-list t)))
          (should (cl-every (lambda (x) (memq x buffers)) (list a b c)))))))

  (ert-deftest get-real-buffers ()
    (with-temp-buffers! (a b c d)
      (with-current-buffer c
        (rename-buffer "*C*"))
      (with-current-buffer d
        (doom-popup-mode +1))
      (let ((buffers (doom-real-buffers-list buffer-list)))
        (should (= (length buffers) 2))
        (should (cl-every  (lambda (x) (memq x buffers)) (list a b)))
        (should (cl-notany (lambda (x) (memq x buffers)) (list c d))))))

  (ert-deftest kill-buffers ()
    (with-temp-buffers! (a b)
      (doom-kill-buffer a)
      (should-not (buffer-live-p a))
      ;; modified buffer
      (with-current-buffer b
        (set-buffer-modified-p t))
      (doom-kill-buffer b t)
      (should-not (buffer-live-p a))))

  (ert-deftest kill-buffer-then-show-real-buffer ()
    (with-temp-buffers! (a b c)
      (should (cl-every #'buffer-live-p buffer-list))
      (switch-to-buffer a)
      (should (eq (current-buffer) a))
      (should (eq (selected-window) (get-buffer-window a)))
      (should (doom-kill-buffer a))
      (should (eq (current-buffer) b))
      (should (doom-kill-buffer))
      (should (eq (current-buffer) c))
      (doom/kill-this-buffer)
      (should (eq (current-buffer) (doom-fallback-buffer)))))

  (ert-deftest matching-buffers ()
    (with-temp-buffers! (a b c)
      (let ((buffers (doom-matching-buffers "^[ac]$")))
        (should (= 2 (length buffers)))
        (should (cl-every #'bufferp buffers))
        (should (cl-every (lambda (x) (memq x buffers)) (list a c)))
        (should (equal (reverse buffers) (doom-matching-buffers "^[ac]$" buffer-list))))))

  (ert-deftest buffers-in-mode ()
    (with-temp-buffers! (a b c d e)
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
        (should (= 3 (length txt-buffers)))))))
