;;; lang/fortran/autoload.el -*- lexical-binding: t; -*-

;; --- GFORTRAN --- ;;

;;;###autoload
(defun +fortran/gfortran-compile ()
  "Compile the current file using gfortran."
  (interactive)
  (message "Compiling with gfortran."))

;;;###autoload
(defun +fortran/gfortran-run ()
  "Run the current file using gfortran."
  (interactive)
  (message "Running with gfortran."))

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
