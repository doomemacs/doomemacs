;;; ui/pretty-code/autoload/install.el -*- lexical-binding: t; -*-
;;;###if (or (featurep! +fira) (featurep! +hasklig) (featurep! +iosevka))

(defvar +pretty-code-font-alist
  '(("Fira Code"
     :url "https://github.com/tonsky/FiraCode/raw/13234c0/distr/ttf/%s"
     :files ("FiraCode-Bold.ttf"
             "FiraCode-Light.ttf"
             "FiraCode-Medium.ttf"
             "FiraCode-Regular.ttf"
             "FiraCode-Retina.ttf"))
    ("Hasklig"
     :url "https://github.com/jsravn/hasklig-emacs/raw/33354a3/%s"
     :files ("Hasklig-Black.otf"
             "Hasklig-BlackIt.otf"
             "Hasklig-Bold.otf"
             "Hasklig-BoldIt.otf"
             "Hasklig-ExtraLight.otf"
             "Hasklig-ExtraLightIt.otf"
             "Hasklig-It.otf"
             "Hasklig-Light.otf"
             "Hasklig-LightIt.otf"
             "Hasklig-Medium.otf"
             "Hasklig-MediumIt.otf"
             "Hasklig-Regular.otf"
             "Hasklig-Semibold.otf"
             "Hasklig-SemiboldIt.otf"))
    ("Iosevka"
     :url "https://github.com/jsravn/iosevka-emacs/raw/20fc2c4/%s"
     :files ("iosevka-custom-lightoblique.ttf"
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
             "iosevka-custom-regular.ttf")))
  "An alist of font metadata for `+pretty-code/install-font'.")

(defun +pretty-code--install-font (prefix name url-format fonts-alist &optional extra-fonts)
  "Install fonts to the local system.

If PREFIX is nil, will prompt whether or not to download. NAME is informational
only. URL-FORMAT is a format string that should be a url and have a single %s,
which is expanded for each font in FONTS-ALIST. FONTS-ALIST should be the
filename of each font. It is used as the source and destination filename."
  (unless (or prefix
              (yes-or-no-p
               (format "This will download and install the %s fonts, continue?"
                       name)))
    (user-error "Aborted"))
  (let* ((font-dest
          (cond (IS-LINUX
                 (expand-file-name
                  "fonts/" (or (getenv "XDG_DATA_HOME")
                               "~/.local/share")))
                (IS-MAC
                 (expand-file-name "~/Library/Fonts/"))))
         (known-dest-p (stringp font-dest))
         (font-dest (or font-dest (read-directory-name "Font installation directory: " "~/"))))
    (unless (file-directory-p font-dest)
      (mkdir font-dest t))
    (dolist (font fonts-alist)
      (url-copy-file (format url-format font)
                     (expand-file-name font font-dest)
                     t))
    (when known-dest-p
      (message "Font downloaded, updating font cache... <fc-cache -f -v> ")
      (shell-command-to-string "fc-cache -f -v"))
    (message "Successfully %s `%s' fonts to `%s'!"
             (if known-dest-p "installed" "downloaded")
             name font-dest)))

;;;###autoload
(defun +pretty-code/install-patched-font (font-name &optional prefix)
  "Install FONT-NAME on your system.
When PREFIX is non-nil, ignore the prompt and just install it."
  (interactive
   (list (completing-read
          "Install font: "
          (mapcar #'car +pretty-code-font-alist))))
  (if-let (font-files (cdr (assoc font-name +pretty-code-font-alist)))
      (cl-destructuring-bind (&key url files) font-files
        (+pretty-code--install-font prefix font-name url files))
    (user-error "%S is not a valid font")))
