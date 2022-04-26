;;; lang/fortran/autoload.el -*- lexical-binding: t; -*-

;;
;;; Generalised Building

;;;###autoload
(defun +fortran/build ()
  "Compile a Fortran project or file.

If the current file is detected to be within an fpm project, then
building will occur with fpm. Otherwise it will default to ifort
or gfortran, depending on what feature flags are set."
  (interactive)
  (cond ((+fortran--fpm-toml) (+fortran/fpm-build))
        ((featurep! +intel) (+fortran/ifort-compile))
        (t (+fortran/gfortran-compile))))

;;;###autoload
(defun +fortran/run ()
  "Run a Fortran project or file.

If the current file is detected to be within an fpm project, then
building will occur with fpm. Otherwise it will default to ifort
or gfortran, depending on what feature flags are set."
  (interactive)
  (cond ((+fortran--fpm-toml) (+fortran/fpm-run))
        ((featurep! +intel) (+fortran/ifort-run))
        (t (+fortran/gfortran-run))))

;; Intel Fortran
;;;###autoload
(defun +fortran/ifort-compile ()
  "Compile the current buffer using ifort."
  (interactive)
  (compile (format "ifort %S"
                   (buffer-file-name))))

;;;###autoload
(defun +fortran/ifort-run ()
  "Run the current buffer using ifort."
  (interactive)
  (delete-file "./a.out")
  (+fortran/ifort-compile)
  (while (not (file-exists-p "./a.out"))
    (sleep-for 1))
  (compile "./a.out"))

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
  (compile (format "gfortran %s %S"
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
(defun +fortran--fpm-toml ()
  "If this is an fpm project, find its toml file."
  (when-let* ((project-root (doom-project-root))
              (toml (expand-file-name "fpm.toml" project-root)))
    (when (file-exists-p toml)
      toml)))

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
