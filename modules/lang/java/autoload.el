;;; lang/java/config.el -*- lexical-binding: t; -*-

;; yasnippet defuns
;;;###autoload
(defun +java-android-mode-is-layout-file ()
  (and android-mode
       (eq major-mode 'nxml-mode)
       (string-equal (file-name-base (directory-file-name default-directory)) "layout")))

;;;###autoload
(defun +java-android-mode-in-tags (&rest tags)
  (cl-find (android-mode-tag-name) tags))

;;;###autoload
(defun +java-android-mode-tag-name ()
  (save-excursion
    (let (beg end)
      (nxml-backward-up-element)
      (evil-forward-word-begin)
      (setq beg (point))
      (evil-forward-WORD-end)
      (setq end (1+ (point)))
      (buffer-substring-no-properties beg end))))

;;;###autoload
(defun +java|android-mode-maybe ()
  (when (project-file-exists-p! (or "local.properties"
                                    "AndroidManifest.xml"
                                    "src/main/AndroidManifest.xml"))
    (android-mode +1)
    (doom/set-build-command "./gradlew %s" "build.gradle")))

