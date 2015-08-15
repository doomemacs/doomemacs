;;; defuns-java.el ---

;; yasnippet defuns
;;;###autoload
(defun narf/java-android-mode-is-layout-file ()
  (and android-mode
       (eq major-mode 'nxml-mode)
       (string-equal (file-name-base (directory-file-name default-directory)) "layout")))

;;;###autoload
(defun narf/java-android-mode-in-tags (&rest tags)
  (-contains? tags (android-mode-tag-name)))

;;;###autoload
(defun narf/java-android-mode-tag-name ()
  (save-excursion
    (let (beg end)
      (nxml-backward-up-element)
      (evil-forward-word-begin)
      (setq beg (point))
      (evil-forward-WORD-end)
      (setq end (1+ (point)))
      (buffer-substring-no-properties beg end))))

;;;###autoload
(defun narf|android-mode-enable-maybe ()
  (let ((root (narf/project-root)))
    (when (or (narf/project-has-files "local.properties" root)
              (narf/project-has-files "AndroidManifest.xml" root)
              (narf/project-has-files "src/main/AndroidManifest.xml" root))
      (android-mode +1)
      (narf/set-build-command "./gradlew %s" "build.gradle"))))

(provide 'defuns-java)
;;; defuns-java.el ends here
