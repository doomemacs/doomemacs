;;; defuns-ruby.el

;;;###autoload
(defun narf|enable-robe-maybe ()
  (let ((file (buffer-file-name)))
    ;; Don't run in gemfiles, capfiles or vagrantfiles
    (unless (or (member (f-filename file) '("Gemfile" "Capfile" "Vagrantfile"))
                (f-ext? file "org")) ;; or org-mode
      (robe-mode 1)
      (narf|ruby-load-file file))))

;;;###autoload
(defun narf|ruby-load-file (&optional file)
  (let ((file (or file buffer-file-name)))
    (when (and (eq major-mode 'ruby-mode)
               (featurep 'robe)
               (not (string= (f-base file) "Gemfile"))
               (file-exists-p buffer-file-name))
      (unless robe-running (robe-start 1))
      (when robe-running (ruby-load-file file)))))

(provide 'defuns-ruby)
;;; defuns-ruby.el ends here
