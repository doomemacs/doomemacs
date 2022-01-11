;;; lang/fortran/autoload.el -*- lexical-binding: t; -*-

;;
;;; GFortran

(defun +fortran--std ()
  "Which version of Fortran should we target?"
  (pcase major-mode
    (`fortran-mode "-std=legacy")
    (_ "")))

;;;###autoload
(defun +fortran-compilation-buffer-name-fn (mode)
  "The name of the buffer produced by `compile'."
  "*fortran-compilation*")

;;;###autoload
(defun +fortran/gfortran-compile ()
  "Compile the current buffer using gfortran."
  (interactive)
  (compile (format "gfortran %s %s"
                   (+fortran--std)
                   buffer-file-name)))

;;;###autoload
(defun +fortran/gfortran-run ()
  "Run the current buffer using gfortran."
  (interactive)
  (delete-file "./a.out")
  (+fortran/gfortran-compile)
  (while (not (file-exists-p "./a.out"))
    (sleep-for 1))
  (compile "./a.out"))


;;
;;; FPM

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
