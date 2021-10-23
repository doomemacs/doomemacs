;;; lang/fortran/autoload.el -*- lexical-binding: t; -*-

;; --- GFORTRAN --- ;;

;;;###autoload
(defun +fortran/gfortran-compile ()
  "Compile the current file using gfortran."
  (interactive)
  (compile (format "gfortran %s %s"
                   (+fortran/fortran-std)
                   buffer-file-name)))

;;;###autoload
(defun +fortran/gfortran-run ()
  "Run the current file using gfortran."
  (interactive)
  (message "Running with gfortran."))

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
  (message "Building with fpm."))

;;;###autoload
(defun +fortran/fpm-run ()
  "Run the current project using fpm."
  (interactive)
  (message "Running with fpm."))

;;;###autoload
(defun +fortran/fpm-test ()
  "Test the current project using fpm."
  (interactive)
  (message "Testing with fpm."))

;; --- MISC. --- ;;
;;;###autoload
(defun +fortran/compilation-buffer-name (mode)
  "The name of the buffer produced by `compile'."
  (interactive)
  "*fortran-compilation*")
