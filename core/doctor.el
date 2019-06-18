;;; core/doctor.el -*- lexical-binding: t; -*-

(defun file-size (file &optional dir)
  (setq file (expand-file-name file dir))
  (when (file-exists-p file)
    (/ (nth 7 (file-attributes file))
       1024.0)))

;; Check for oversized problem files in cache that may cause unusual/tremendous
;; delays or freezing. This shouldn't happen often.
(dolist (file (list "savehist"
                    "projectile.cache"))
  (let* ((path (expand-file-name file doom-cache-dir))
         (size (file-size path)))
    (when (and (numberp size) (> size 2000))
      (warn! "%s is too large (%.02fmb). This may cause freezes or odd startup delays"
             (file-relative-name path doom-core-dir)
             (/ size 1024))
      (explain! "Consider deleting it from your system (manually)"))))

(unless (executable-find doom-projectile-fd-binary)
  (warn! "Couldn't find the `fd' binary; project file searches will be slightly slower")
  (unless (executable-find "rg")
    (warn! "Couldn't find the `rg' binary either; project file searches will be even slower")))

(let ((default-directory "~"))
  (require 'projectile)
  (when (cl-find-if #'projectile-file-exists-p projectile-project-root-files-bottom-up)
    (warn! "Your $HOME is recognized as a project root")
    (explain! "Doom will disable bottom-up root search, which may reduce the accuracy of project\n"
              "detection.")))

;; There should only be one
(when (and (file-equal-p doom-private-dir "~/.config/doom")
           (file-directory-p "~/.doom.d"))
  (warn! "Both %S and '~/.doom.d' exist on your system"
         (abbreviate-file-name doom-private-dir))
  (explain! "Doom will only load one of these (~/.config/doom takes precedence). Possessing\n"
            "both is rarely intentional; you should one or the other."))

;; Check for fonts
(if (not (fboundp 'find-font))
    (progn
      (warn! "Warning: unable to detect font")
      (explain! "The `find-font' function is missing. This could indicate the incorrect "
                "version of Emacs is being used!"))
  ;; all-the-icons fonts
  (let ((font-dest (pcase system-type
                     (`gnu/linux (concat (or (getenv "XDG_DATA_HOME")
                                             "~/.local/share")
                                         "/fonts/"))
                     (`darwin "~/Library/Fonts/"))))
    (when (and font-dest (require 'all-the-icons nil t))
      (dolist (font all-the-icons-font-families)
        (if (sh "fc-list | grep %s" font)
            (success! "Found font %s" font)
          (warn! "Warning: couldn't find %s font in %s"
                 font font-dest)
          (explain! "You can install it by running `M-x all-the-icons-install-fonts' within Emacs.\n\n"
                    "This could also mean you've installed them in non-standard locations, in which "
                    "case feel free to ignore this warning."))))))
