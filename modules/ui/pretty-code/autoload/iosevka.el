;;; ui/pretty-code/autoload/iosevka.el -*- lexical-binding: t; -*-

;;;###autoload
(defvar +pretty-code--iosevka-font-names
  '(
    "iosevka-custom-lightoblique.ttf"
    "iosevka-custom-thinoblique.ttf"
    "iosevka-custom-mediumitalic.ttf"
    "iosevka-custom-light.ttf"
    "iosevka-custom-heavy.ttf"
    "iosevka-custom-bolditalic.ttf"
    "iosevka-custom-bold.ttf"
    "iosevka-custom-lightitalic.ttf"
    "iosevka-custom-thin.ttf"
    "iosevka-custom-extralight.ttf"
    "iosevka-custom-oblique.ttf"
    "iosevka-custom-italic.ttf"
    "iosevka-custom-heavyoblique.ttf"
    "iosevka-custom-heavyitalic.ttf"
    "iosevka-custom-extralightitalic.ttf"
    "iosevka-custom-thinitalic.ttf"
    "iosevka-custom-medium.ttf"
    "iosevka-custom-mediumoblique.ttf"
    "iosevka-custom-extralightoblique.ttf"
    "iosevka-custom-boldoblique.ttf"
    "iosevka-custom-regular.ttf"))

;;;###autoload
(defun +pretty-code/install-iosevka-font (&optional pfx)
  "Helper function to download and install Iosevka font based on OS.
When PFX is non-nil, ignore the prompt and just install"
  (interactive "P")
  (when (or pfx (yes-or-no-p "This will download and install the Iosevka fonts, are you sure you want to do this?"))
    (let* ((url-format "https://github.com/jsravn/iosevka-emacs/raw/master/%s")
           (font-dest (cl-case window-system
                        (x  (concat (or (getenv "XDG_DATA_HOME")            ;; Default Linux install directories
                                        (concat (getenv "HOME") "/.local/share"))
                                    "/fonts/"))
                        (mac (concat (getenv "HOME") "/Library/Fonts/" ))
                        (ns (concat (getenv "HOME") "/Library/Fonts/" ))))  ;; Default MacOS install directory
           (known-dest? (stringp font-dest))
           (font-dest (or font-dest (read-directory-name "Font installation directory: " "~/"))))

      (unless (file-directory-p font-dest) (mkdir font-dest t))

      (dolist (font +pretty-code--iosevka-font-names)
        (url-copy-file (format url-format font) (expand-file-name font font-dest) t))

      (when known-dest?
        (message "Font downloaded, updating font cache... <fc-cache -f -v> ")
        (shell-command-to-string (format "fc-cache -f -v")))
      (message "Successfully %s `Iosevka' fonts to `%s'!"
               (if known-dest? "installed" "downloaded")
               font-dest))))
