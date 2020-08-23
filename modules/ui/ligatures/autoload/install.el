;;; ui/ligatures/autoload/install.el -*- lexical-binding: t; -*-
;;;###if (or (featurep! +fira) (featurep! +hasklig) (featurep! +iosevka))

(defun +ligatures--install-font (prefix name url-format fonts-alist &optional extra-fonts)
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
    (if IS-WINDOWS
        (when (y-or-n-p "The %S font was downloaded, but Windows users must install them manually.\n\nShow files in windows explorer?")
          (call-process "explorer.exe" nil nil nil font-dest))
      (message "Successfully %s %S fonts to %S!"
               (if known-dest-p "installed" "downloaded")
               name font-dest))))

;;;###autoload
(defun +ligatures/install-patched-font (font-id &optional arg)
  "Install the font FONT-ID on your system.
FONT-ID must be a key from `+ligatures--font-alist'.
If PREFIX is non-nil, don't ask for confirmation and install it."
  (interactive
   (list
    (car (cl-find (completing-read
                   "Install font: "
                   (mapcar #'cadr +ligatures--font-alist))
                  +ligatures--font-alist
                  :key #'cadr
                  :test #'equal))
    current-prefix-arg))
  (cl-destructuring-bind (font-name &key _range url files)
      (or (alist-get font-id +ligatures--font-alist)
          (user-error "%S is not a valid font" font-id))
    (+ligatures--install-font arg font-name url files)))
