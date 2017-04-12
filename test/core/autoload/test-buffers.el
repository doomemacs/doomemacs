;;; test/core/autoload/test-buffers.el

(def-test-group! core/autoload/buffers
  (ert-deftest get-buffers ()
    (let ((a (get-buffer-create "*a*"))
          (b (get-buffer-create "*b*"))
          (c (get-buffer-create "*c*"))
          (buffers (doom-buffer-list)))
      (should buffers)
      (should (cl-every 'bufferp buffers))
      (should (cl-every (lambda (b) (memq b buffers)) (list a b c)))))

  (ert-deftest matching-buffers ()
    (let ((a (get-buffer-create "*a*"))
          (b (get-buffer-create "*b*"))
          (c (get-buffer-create "*c*"))
          (buffers (doom-matching-buffers "^\\*[ac]\\*$")))
      (should (= 2 (length buffers)))
      (should (cl-every 'bufferp buffers))
      (should (cl-every (lambda (b) (memq b buffers)) (list a c)))))

  (ert-deftest buffers-in-mode ()
    (dolist (name (list "*a*" "*b*"))
      (with-current-buffer (get-buffer-create name)
        (emacs-lisp-mode)))
    (dolist (name (list "*c*" "*d*" "*e*"))
      (with-current-buffer (get-buffer-create name)
        (text-mode)))
    (let ((el-buffers (doom-buffers-in-mode 'emacs-lisp-mode))
          (txt-buffers (doom-buffers-in-mode 'text-mode)))
      (should (cl-every 'bufferp (append el-buffers txt-buffers)))
      (should (= 2 (length el-buffers)))
      (should (= 3 (length txt-buffers)))))

  ;; TODO
  )
