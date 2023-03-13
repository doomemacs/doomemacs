;;; lang/elm/autoloads.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +elm/compile-html ()
  "Compile the current Elm project."
  (interactive)
  (let ((default-directory (doom-project-root)))
    (compile (format "elm make %s" buffer-file-name))))

;;;###autoload
(defun +elm/compile-html-optimized ()
  "Compile the current Elm project with optimizations."
  (interactive)
  (let ((default-directory (doom-project-root)))
    (compile (format "elm make %s --optimize" buffer-file-name))))

;;;###autoload
(defun +elm/compile-js ()
  "Compile the current Elm project to Javascript."
  (interactive)
  (let ((default-directory (doom-project-root)))
    (compile (format "elm make %s --output=main.js" buffer-file-name))))

;;;###autoload
(defun +elm/compile-js-optimized ()
  "Compile the current Elm project to Javascript with optimizations."
  (interactive)
  (let ((default-directory (doom-project-root)))
    (compile (format "elm make %s --output=main.js --optimize" buffer-file-name))))
