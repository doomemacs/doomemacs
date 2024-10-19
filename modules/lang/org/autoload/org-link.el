;;; lang/org/autoload/org-link.el -*- lexical-binding: t; -*-

(defun +org--relative-path (path root)
  (if (and buffer-file-name (file-in-directory-p buffer-file-name root))
      (file-relative-name path)
    path))

(defun +org--read-link-path (key dir &optional fn)
  (let ((file (funcall (or fn #'read-file-name) (format "%s: " (capitalize key)) dir)))
    (format "%s:%s" key (file-relative-name file dir))))

;;;###autoload
(defun +org-define-basic-link (key dir-var &rest plist)
  "Define a link with some basic completion & fontification.

KEY is the name of the link type. DIR-VAR is the directory variable to resolve
links relative to. PLIST is passed to `org-link-set-parameters' verbatim.

Links defined with this will be rendered in the `error' face if the file doesn't
exist, and `org-link' otherwise."
  (declare (indent 2))
  (let ((requires (plist-get plist :requires))
        (dir-fn (if (functionp dir-var)
                    dir-var
                  (lambda () (symbol-value dir-var)))))
    (apply #'org-link-set-parameters
           key
           :complete (lambda ()
                       (if requires (mapc #'require (ensure-list requires)))
                       (+org--relative-path (+org--read-link-path key (funcall dir-fn))
                                            (funcall dir-fn)))
           :follow   (lambda (link)
                       (org-link-open-as-file (expand-file-name link (funcall dir-fn)) nil))
           :face     (lambda (link)
                       (let* ((path (expand-file-name link (funcall dir-fn)))
                              (option-index (string-match-p "::\\(.*\\)\\'" path))
                              (file-name (substring path 0 option-index)))
                         (if (file-exists-p file-name)
                             'org-link
                           'error)))
           (plist-put plist :requires nil))))

;;;###autoload
(defun +org-link-read-desc-at-point (&optional default context)
  "TODO"
  (if (and (stringp default) (not (string-empty-p default)))
      (string-trim default)
    (if-let* ((context (or context (org-element-context)))
              (context (org-element-lineage context '(link) t))
              (beg (org-element-property :contents-begin context))
              (end (org-element-property :contents-end context)))
        (unless (= beg end)
          (replace-regexp-in-string
           "[ \n]+" " " (string-trim (buffer-substring-no-properties beg end)))))))

;;;###autoload
(defun +org-link-read-kbd-at-point (&optional default context)
  "TODO"
  (+org-link--describe-kbd
   (+org-link-read-desc-at-point default context)))

(defun +org-link--describe-kbd (keystr)
  (dolist (key `(("<leader>" . ,doom-leader-key)
                 ("<localleader>" . ,doom-localleader-key)
                 ("<prefix>" . ,(if (bound-and-true-p evil-mode)
                                    (concat doom-leader-key " u")
                                  "C-u"))
                 ("<help>" . ,(if (bound-and-true-p evil-mode)
                                  (concat doom-leader-key " h")
                                "C-h"))
                 ("\\<M-" . "alt-")
                 ("\\<S-" . "shift-")
                 ("\\<s-" . "super-")
                 ("\\<C-" . "ctrl-")))
    (setq keystr
          (replace-regexp-in-string (car key) (cdr key)
                                    keystr t t)))
  keystr)

(defun +org-link--read-module-spec (module-spec-str)
  (if (string-prefix-p "+" (string-trim-left module-spec-str))
      (let ((title (cadar (org-collect-keywords '("TITLE")))))
        (if (and title (string-match-p "\\`:[a-z]+ [A-Za-z0-9]+\\'" title))
            (+org-link--read-module-spec (concat title " " module-spec-str))
          (list :category nil :module nil :flag (intern module-spec-str))))
    (cl-destructuring-bind (category &optional module flag)
        (mapcar #'intern (split-string
                          (if (string-prefix-p ":" module-spec-str)
                              module-spec-str
                            (concat ":" module-spec-str))
                          "[ \n]+" nil))
      (list :category category
            :module module
            :flag flag))))

;;;###autoload
(defun +org-link--var-link-activate-fn (start end var _bracketed-p)
  (when buffer-read-only
    (add-text-properties
     start end
     (list
      'display
      (concat (nerd-icons-mdicon "nf-md-toggle_switch") ; "󰔡"
              " " (propertize var
                              'face
                              (if (boundp (intern var))
                                  'font-lock-variable-name-face
                                'shadow)))))))

;;;###autoload
(defun +org-link--fn-link-activate-fn (start end fn _bracketed-p)
  (when buffer-read-only
    (add-text-properties
     start end
     (list 'display
           (concat (nerd-icons-mdicon "nf-md-function") ; "󰊕"
                   " " (propertize fn
                                   'face
                                   (if (fboundp (intern fn))
                                       'font-lock-function-name-face
                                     'shadow)))))))

;;;###autoload
(defun +org-link--face-link-activate-fn (start end face _bracketed-p)
  (when buffer-read-only
    (add-text-properties
     start end
     (list 'display
           (concat (nerd-icons-mdicon "nf-md-format_text") ; "󰊄"
                   " " (propertize face
                                   'face
                                   (if (facep (intern face))
                                       (intern face)
                                     'shadow)))))))

(defun +org-link--command-keys (command)
  "Convert command reference TEXT to key binding representation."
  (let* ((cmd-prefix (mapcar #'reverse
                             (split-string (reverse command) " ")))
         (cmd (intern-soft (car cmd-prefix)))
         (prefix (and (cdr cmd-prefix)
                      (string-join (reverse (cdr cmd-prefix)) " ")))
         (key-binding (where-is-internal cmd nil t))
         (key-text (if key-binding
                       (key-description key-binding)
                     (format "M-x %s" cmd))))
    (concat prefix (and prefix " ") key-text)))

;;;###autoload
(defun +org-link--command-link-activate-fn (start end command _bracketed-p)
  (when buffer-read-only
    (add-text-properties
     start end (list 'display (+org-link--command-keys command)))))

;;;###autoload
(defun +org-link--doom-module-link-follow-fn (module-path _arg)
  (cl-destructuring-bind (&key category module flag)
      (+org-link--read-module-spec module-path)
    (when category
      (if-let* ((path (doom-module-locate-path (cons category module)))
                (path (or (car (doom-glob path "README.org"))
                          path)))
          (find-file path)
        (user-error "Can't find Doom module '%s'" module-path)))
    (when flag
      (goto-char (point-min))
      (when (and (re-search-forward "^\\*+ \\(?:TODO \\)?Module flags")
                 (re-search-forward (format "^\\s-*- \\+%s ::[ \n]"
                                            (substring (symbol-name flag) 1))
                                    (save-excursion (org-get-next-sibling)
                                                    (point))))
        (org-show-entry)
        (recenter)))))

;;;###autoload
(defun +org-link--doom-module-link-activate-fn (start end module-path _bracketed-p)
  (when buffer-read-only
    (cl-destructuring-bind (&key category module flag)
        (+org-link--read-module-spec module-path)
      (let ((overall-face
             (if (and category (doom-module-locate-path (cons category module)))
                 '((:underline nil) org-link org-block bold)
               '(shadow org-block bold)))
            (icon-face
             (cond
              ((doom-module-active-p category module flag) 'success)
              ((and category (doom-module-locate-path (cons category module))) 'warning)
              (t 'error))))
        (add-text-properties
         start end
         (list 'face overall-face
               'display
               (concat
                (nerd-icons-octicon "nf-oct-stack" ; ""
                                    :face icon-face)
                " " module-path)))))))

;;;###autoload
(defun +org-link--doom-package-link-activate-fn (start end package _bracketed-p)
  (when buffer-read-only
    (let ((overall-face
           (if (locate-library package)
               '((:underline nil :weight regular) org-link org-block italic)
             '(shadow org-block italic)))
          (icon-face
           (cond
            ((featurep (intern package)) 'success)
            ((locate-library package) 'warning)
            (t 'error))))
      (add-text-properties
       start end
       (list 'face overall-face
             'display
             (concat
              (nerd-icons-octicon "nf-oct-package" ; ""
                                  :face icon-face)
              " " package))))))

;;;###autoload
(defun +org-link--doom-package-link-follow-fn (pkg _prefixarg)
  "TODO"
  (doom/describe-package (intern-soft pkg)))

;;;###autoload
(defun +org-link--doom-executable-link-activate-fn (start end executable _bracketed-p)
  (when buffer-read-only
    (let ((found (executable-find executable)))
      (add-text-properties
       start end
       (list 'display
             (concat
              (nerd-icons-octicon "nf-oct-terminal" ; ""
                                  :face (if found 'success 'error))
              " "
              (propertize executable
                          'face (if found 'org-verbatim 'shadow))))))))

;;
;;; Help-echo / eldoc

(defadvice! doom-docs--display-docs-link-in-eldoc-a (&rest _)
  "Display help for doom-*: links in minibuffer when cursor/mouse is over it."
  :before-until #'org-eldoc-documentation-function
  (and (bound-and-true-p doom-docs-mode)
       (eq (get-text-property (point) 'help-echo)
           #'+org-link-doom--help-echo-from-textprop)
       (+org-link-doom--help-echo-from-textprop nil (current-buffer) (point))))

(defvar +org-link-doom--help-echo-cache nil)

(defun +org-link-doom--help-echo-from-textprop (_window object pos)
  (let ((link (with-current-buffer object
                (save-excursion (goto-char pos) (org-element-context)))))
    (if (eq (car +org-link-doom--help-echo-cache)
            (org-element-property :begin link))
        (cdr +org-link-doom--help-echo-cache)
      (cdr (setq +org-link-doom--help-echo-cache
                 (cons (org-element-property :begin link)
                       (+org-link-doom--help-string link)))))))

(defun +org-link-doom--help-string (link)
  (if (not buffer-read-only)
      (format "LINK: %s" (org-element-property :raw-link link))
    (pcase (org-element-property :type link)
      ("kbd"
       (concat
        "The key sequence "
        (propertize (+org-link--describe-kbd (org-element-property :path link))
                    'face 'help-key-binding)))
      ("cmd"
       (concat
        "The command "
        (propertize (org-element-property :path link) 'face 'font-lock-function-name-face)
        " can be invoked with the key sequence "
        (propertize (+org-link--command-keys (org-element-property :path link))
                    'face 'help-key-binding)))
      ("doom-package"
       (concat
        (propertize "Emacs package " 'face 'bold)
        (propertize (org-element-property :path link) 'face 'font-lock-keyword-face)
        ", currently "
        (cond
         ((featurep (intern-soft (org-element-property :path link)))
          (propertize "installed and loaded" 'face 'success))
         ((locate-library (org-element-property :path link))
          (propertize "installed but not loaded" 'face 'warning))
         (t (propertize "not installed" 'face 'error )))))
      ("doom-module"
       (concat
        (propertize "Doom module " 'face 'bold)
        (propertize (org-element-property :path link) 'face 'font-lock-keyword-face)
        ", currently "
        (cl-destructuring-bind (&key category module flag)
            (+org-link--read-module-spec (org-element-property :path link))
          (cond
           ((doom-module-active-p category module)
            (propertize "enabled" 'face 'success))
           ((and category (doom-module-locate-path (cons category module)))
            (propertize "disabled" 'face 'error))
           (t (propertize "unknown" 'face '(bold error)))))))
      ("doom-executable"
       (concat
        (propertize "System executable " 'face 'bold)
        (propertize (org-element-property :path link) 'face 'font-lock-keyword-face)
        ", "
        (if (executable-find (org-element-property :path link))
            (propertize "found" 'face 'success)
          (propertize "not found" 'face 'error))
        " on PATH")))))

;;
;;; Image data functions (for custom inline images)

;;;###autoload
(defun +org-image-file-data-fn (protocol link _description)
  "Intepret LINK as an image file path and return its data."
  (setq
   link (pcase protocol
          ("download"
           (expand-file-name link (or (if (require 'org-download nil t) org-download-image-dir)
               default-directory)))
          ("attachment"
           (require 'org-attach)
           (org-attach-expand link))
          (_ (expand-file-name link default-directory))))
  (when (and (file-exists-p link)
             (image-type-from-file-name link))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (setq buffer-file-coding-system 'binary)
      (insert-file-contents-literally link)
      (buffer-substring-no-properties (point-min) (point-max)))))

;;;###autoload
(defun +org-inline-image-data-fn (_protocol link _description)
  "Interpret LINK as base64-encoded image data."
  (base64-decode-string link))

;;;###autoload
(defun +org-http-image-data-fn (protocol link _description)
  "Interpret LINK as an URL to an image file."
  (when (and (image-type-from-file-name link)
             (not (eq org-display-remote-inline-images 'skip)))
    (if-let (buf (url-retrieve-synchronously (concat protocol ":" link)))
        (with-current-buffer buf
          (goto-char (point-min))
          (re-search-forward "\r?\n\r?\n" nil t)
          (buffer-substring-no-properties (point) (point-max)))
      (message "Download of image \"%s\" failed" link)
      nil)))

(defvar +org--gif-timers nil)
;;;###autoload
(defun +org-play-gif-at-point-h ()
  "Play the gif at point, while the cursor remains there (looping)."
  (dolist (timer +org--gif-timers (setq +org--gif-timers nil))
    (when (timerp (cdr timer))
      (cancel-timer (cdr timer)))
    (image-animate (car timer) nil 0))
  (when-let* ((ov (cl-find-if
                   (lambda (it) (overlay-get it 'org-image-overlay))
                   (overlays-at (point))))
              (dov (overlay-get ov 'display))
              (pt  (point)))
    (when (image-animated-p dov)
      (push (cons
             dov (run-with-idle-timer
                  0.5 nil
                  (lambda (dov)
                    (when (equal
                           ov (cl-find-if
                               (lambda (it) (overlay-get it 'org-image-overlay))
                               (overlays-at (point))))
                      (message "playing gif")
                      (image-animate dov nil t)))
                  dov))
            +org--gif-timers))))

;;;###autoload
(defun +org-play-all-gifs-h ()
  "Continuously play all gifs in the visible buffer."
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when-let* (((overlay-get ov 'org-image-overlay))
                (dov (overlay-get ov 'display))
                ((image-animated-p dov))
                (w (selected-window)))
      (while-no-input
        (run-with-idle-timer
         0.3 nil
         (lambda (dov)
           (when (pos-visible-in-window-p (overlay-start ov) w nil)
             (unless (plist-get (cdr dov) :animate-buffer)
               (image-animate dov))))
         dov)))))


;;
;;; Commands

;;;###autoload
(defun +org/remove-link ()
  "Unlink the text at point."
  (interactive)
  (unless (org-in-regexp org-link-bracket-re 1)
    (user-error "No link at point"))
  (save-excursion
    (let ((label (if (match-end 2)
                     (match-string-no-properties 2)
                   (org-link-unescape (match-string-no-properties 1)))))
      (delete-region (match-beginning 0) (match-end 0))
      (insert label))))

;;;###autoload
(defun +org/yank-link ()
  "Copy the url at point to the clipboard.
If on top of an Org link, will only copy the link component."
  (interactive)
  (let ((url (thing-at-point 'url)))
    (kill-new (or url (user-error "No URL at point")))
    (message "Copied link: %s" url)))

;;;###autoload
(defun +org/play-gif-at-point ()
  "TODO"
  (interactive)
  (unless (eq 'org-mode major-mode)
    (user-error "Not in org-mode"))
  (or (+org-play-gif-at-point-h)
      (user-error "No gif at point")))
