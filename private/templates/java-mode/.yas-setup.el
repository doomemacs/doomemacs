(defun yas-java-project-package ()
  (if (eq major-mode 'java-mode)
    (s-chop-suffix "." (s-replace "/" "." (f-dirname (f-relative (buffer-file-name)
                                                                 (concat (narf/project-root) "/src/")))))
    ""))

(defun yas-java-class-name ()
  (if (eq major-mode 'java-mode)
      (f-no-ext (f-base (buffer-file-name)))
    ""))

