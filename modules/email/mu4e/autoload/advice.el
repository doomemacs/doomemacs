;;; email/mu4e/autoload/advice.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +mu4e~main-action-str-prettier-a (str &optional func-or-shortcut)
  "Highlight the first occurrence of [.] in STR.
If FUNC-OR-SHORTCUT is non-nil and if it is a function, call it
when STR is clicked (using RET or mouse-2); if FUNC-OR-SHORTCUT is
a string, execute the corresponding keyboard action when it is
clicked."
  (let ((newstr
         (replace-regexp-in-string
          "\\[\\(..?\\)\\]"
          (lambda(m)
            (format "%s"
                    (propertize (match-string 1 m) 'face 'mu4e-highlight-face)))
          (replace-regexp-in-string "\t\\*" (format "\t%s" +mu4e-main-bullet) str)))
        (map (make-sparse-keymap))
        (func (if (functionp func-or-shortcut)
                  func-or-shortcut
                (if (stringp func-or-shortcut)
                    (lambda()(interactive)
                      (execute-kbd-macro func-or-shortcut))))))
    (define-key map [mouse-2] func)
    (define-key map (kbd "RET") func)
    (put-text-property 0 (length newstr) 'keymap map newstr)
    (put-text-property (string-match "[A-Za-z].+$" newstr)
                       (- (length newstr) 1) 'mouse-face 'highlight newstr)
    newstr))

;;;###autoload
(defun +mu4e~main-keyval-str-prettier-a (str)
  "Replace '*' with `+mu4e-main-bullet' in STR."
  (replace-regexp-in-string "\t\\*" (format "\t%s" +mu4e-main-bullet) str))

;; Org msg LaTeX image scaling

;;;###autoload
(defun +org-msg-img-scale-css (img-uri)
  "For a given IMG-URI, use imagemagick to find its width."
  (if +org-msg-currently-exporting
      (when (and (not IS-WINDOWS)) ; relies on posix path
        (let ((width-call (and (executable-find "identify")
                               (doom-call-process "identify" "-format" "%w"
                                                  (substring img-uri 7))))) ; 7=(length "file://")
          (unless width-call
            (setq width-call (doom-call-process "file" (substring img-uri 7)))
            (setcdr width-call (replace-regexp-in-string "^.*image data, \\([0-9]+\\).*$" "\\1" (cdr width-call)))
            (setcar width-call (if (< 0 (string-to-number (cdr width-call))) 0 1)))
          (when (= (car width-call) 0)
            (list :width
                  (format "%.1fpx"
                          (/ (string-to-number (cdr width-call))
                             (plist-get org-format-latex-options :scale)))))))
    (list :style (format "transform: scale(%.3f)"
                         (/ 1.0 (plist-get org-format-latex-options :scale))))))

;;;###autoload
(defun +org-html-latex-fragment-scaled-a (latex-fragment _contents info)
  "Transcode a LATEX-FRAGMENT object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information.

This differs from `org-html-latex-fragment' in that it uses the LaTeX fragment
as a meaningful alt value, applies a class to indicate what sort of fragment it is
(latex-fragment-inline or latex-fragment-block), and (on Linux) scales the image to
account for the value of :scale in `org-format-latex-options'."
  (let ((latex-frag (org-element-property :value latex-fragment))
        (processing-type (plist-get info :with-latex)))
    (cond
     ((memq processing-type '(t mathjax))
      (org-html-format-latex latex-frag 'mathjax info))
     ((eq processing-type 'html)
      (org-html-format-latex latex-frag 'html info))
     ((assq processing-type org-preview-latex-process-alist)
      (let ((formula-link
             (org-html-format-latex latex-frag processing-type info)))
        (when (and formula-link (string-match "file:\\([^]]*\\)" formula-link))
          (let* ((source (org-export-file-uri (match-string 1 formula-link)))
                 (attributes (append (list :alt latex-frag
                                           :class
                                           (concat "latex-fragment-"
                                                   (if (equal "\\(" (substring latex-frag 0 2))
                                                       "inline" "block")))
                                     (when (memq processing-type '(dvipng convert))
                                       (+org-msg-img-scale-css source)))))
            (org-html--format-image source attributes info)))))
     (t latex-frag))))

;;;###autoload
(defun +org-html-latex-environment-scaled-a (latex-environment _contents info)
  "Transcode a LATEX-ENVIRONMENT element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information.

This differs from `org-html-latex-environment' in that (on Linux) it
scales the image to account for the value of :scale in `org-format-latex-options'."
  (let ((processing-type (plist-get info :with-latex))
        (latex-frag (org-remove-indentation
                     (org-element-property :value latex-environment)))
        (attributes (org-export-read-attribute :attr_html latex-environment))
        (label (and (org-element-property :name latex-environment)
                    (org-export-get-reference latex-environment info)))
        (caption (and (org-html--latex-environment-numbered-p latex-environment)
                      (number-to-string
                       (org-export-get-ordinal
                        latex-environment info nil
                        (lambda (l _)
                          (and (org-html--math-environment-p l)
                               (org-html--latex-environment-numbered-p l))))))))
    (plist-put attributes :class "latex-environment")
    (cond
     ((memq processing-type '(t mathjax))
      (org-html-format-latex
       (if (org-string-nw-p label)
           (replace-regexp-in-string "\\`.*"
                                     (format "\\&\n\\\\label{%s}" label)
                                     latex-frag)
         latex-frag)
       'mathjax info))
     ((assq processing-type org-preview-latex-process-alist)
      (let ((formula-link
             (org-html-format-latex
              (org-html--unlabel-latex-environment latex-frag)
              processing-type info)))
        (when (and formula-link (string-match "file:\\([^]]*\\)" formula-link))
          (let ((source (org-export-file-uri (match-string 1 formula-link))))
            (org-html--wrap-latex-environment
             (org-html--format-image source
                                     (append attributes
                                             (when (memq processing-type '(dvipng convert))
                                               (+org-msg-img-scale-css source)))
                                     info)
             info caption label)))))
     (t (org-html--wrap-latex-environment latex-frag info caption label)))))
