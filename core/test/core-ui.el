;; -*- no-byte-compile: t; -*-
;;; ../core/test/core-ui.el

(defmacro with-temp-windows!! (&rest body)
  (declare (indent defun))
  `(progn
     (delete-other-windows)
     (cl-flet ((split-window (symbol-function #'split-window-horizontally)))
       (let ((a (get-buffer-create "a"))
             (b (get-buffer-create "b"))
             (split-width-threshold 0)
             (window-min-width 0))
         ,@body))))

;;
(def-test! set-mode-name
  (let ((doom-major-mode-names '((text-mode . "abc")
                                 (lisp-mode . (lambda () "xyz"))
                                 (js-mode . t))))
    (text-mode)
    (should (equal mode-name "abc"))
    (lisp-mode)
    (should (equal mode-name "xyz"))
    (should-error (js-mode))))

(def-test! protect-visible-buffers
  (with-temp-windows!!
    (let ((kill-buffer-query-functions '(doom|protect-visible-buffers)))
      (switch-to-buffer a) (split-window)
      (switch-to-buffer b) (split-window)
      (switch-to-buffer a)
      (should-not (kill-buffer))
      (select-window (get-buffer-window b))
      (should (kill-buffer)))))

(def-test! *quit-window
  (with-temp-windows!!
    (let (kill-buffer-query-functions)
      (switch-to-buffer a) (split-window)
      (switch-to-buffer b)
      (save-window-excursion
        (quit-window t)
        (should (buffer-live-p b)))
      (quit-window)
      (should-not (buffer-live-p b)))))
