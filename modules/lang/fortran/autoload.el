;;; lang/fortran/autoload.el -*- lexical-binding: t; -*-

;; --- GFORTRAN --- ;;

;;;###autoload
(defun +fortran/gfortran-compile ()
  "Compile the current file using gfortran."
  (interactive)
  (compile (format "gfortran %s %s" (+fortran/fortran-std) buffer-file-name)))

;;;###autoload
(defun +fortran/gfortran-run ()
  "Run the current file using gfortran."
  (interactive)
  (+fortran/gfortran-compile)
  (let* ((buffer (+fortran/compilation-buffer-name nil))
         (proc (get-buffer-process buffer))
         (exec (expand-file-name "a.out" ".")))
    (while (accept-process-output proc))
    (start-process "gfortran-run" buffer exec)))
    ;; (comint-exec buffer "gfortran-run" exec nil nil)))
    ;; (comint-send-string (get-buffer-process buffer) exec)))

(defun +fortran/fortran-std ()
  "Which version of Fortran should we target?"
  (cl-case major-mode
    (fortran-mode "-std=legacy")
    (t "")))

;; --- FPM --- ;;

;;;###autoload
(defun +fortran/fpm-build ()
  "Build the current project using fpm."
  (interactive)
  (compile "fpm build"))

;;;###autoload
(defun +fortran/fpm-run ()
  "Run the current project using fpm."
  (interactive)
  (compile "fpm run"))

;;;###autoload
(defun +fortran/fpm-test ()
  "Test the current project using fpm."
  (interactive)
  (compile "fpm test"))

;; --- MISC. --- ;;
;;;###autoload
(defun +fortran/compilation-buffer-name (mode)
  "The name of the buffer produced by `compile'."
  (interactive)
  "*fortran-compilation*")
