;;; lang/org/+export.el -*- lexical-binding: t; -*-

(add-hook 'org-load-hook #'+org|init-export)

;; I don't have any beef with org's built-in export system, but I do wish it
;; would export to a central directory (by default), rather than
;; `default-directory'. This is because all my org files are usually in one
;; place, and I want to be able to refer back to old exports if needed.


(defvar +org-html-embed-image t
  "whether image is embeded as base64 in html")

(def-package! htmlize
  :commands (htmlize-buffer
             htmlize-file
             htmlize-many-files
             htmlize-many-files-dired)
  :config
  (setq-default htmlize-pre-style t))

(def-package! ox-pandoc
  :defer t
  :config
  (push 'pandoc org-export-backends)
  (if +org-html-embed-image
      (push '(self-contained . t) org-pandoc-options))
  (setq org-pandoc-options
        '((standalone . t)
          (mathjax . t)
          (parse-raw . t))))

;;
(defun +org|init-export ()
  (setq org-export-backends '(ascii html latex md)
        org-publish-timestamp-directory (concat doom-cache-dir "/org-timestamps/"))

  (when (executable-find "pandoc")
    (require 'ox-pandoc))


  (defun +org*org-html--format-image (source attributes info)
    "Optionally embed image into html as base64."
    (let ((source
           (replace-regexp-in-string "file://" ""
                                     (replace-regexp-in-string
                                      "%20" " " source nil 'literal)))) ;; not sure whether this is necessary
      (if (string= "svg" (file-name-extension source))
          (org-html--svg-image source attributes info)
        (if +org-html-embed-image
            (org-html-close-tag
             "img"
             (format "src=\"data:image/%s;base64,%s\"%s %s"
                     (or (file-name-extension source) "")
                     (base64-encode-string
                      (with-temp-buffer
                        (insert-file-contents-literally (expand-file-name source))
                        (buffer-string)))
                     (file-name-nondirectory source)
                     (org-html--make-attribute-string
                      attributes))
             info)
          (org-html-close-tag
           "img"
           (org-html--make-attribute-string
            (org-combine-plists
             (list :src source
                   :alt (if (string-match-p "^ltxpng/" source)
                            (org-html-encode-plain-text
                             (org-find-text-property-in-string 'org-latex-src source))
                          (file-name-nondirectory source)))
             attributes))
           info)))))
  (advice-add #'org-html--format-image :override #'+org*org-html--format-image)
  (when (featurep! +style)
    (defvar +org-html-export-style-dir (concat doom-modules-dir "lang/org-private/org-html-head")
      "Directory that contains files to be embeded into org export html.")
    (defvar +org-html-export-style-list '("include.html"
                                           "bootstrap-toc.js"
                                           "bootstrap-toc.css"
                                           "org.js"
                                           "org.css")
      "a list of scripts to be included in org-html-head")

    (setq org-html-checkbox-type 'html
          org-html-mathjax-options
          '((path "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.2/MathJax.js?config=TeX-AMS_SVG")
            (scale "100")
            (align "center")
            (font "TeX")
            (linebreaks "false")
            (autonumber "AMS")
            (indent "0em")
            (multlinewidth "85%")
            (tagindent ".8em")
            (tagside "right"))
          org-html-table-default-attributes
          '(:border "2"
                    :class "table table-striped table-sm table-bordered"
                    :cellspacing "0"
                    :cellpadding "6"
                    :rules "groups"
                    :frame "hsides"))

    (defun +org/org--embed-html (file)
      "Convert html files to string for embedding"
      (concat
       (with-temp-buffer
         (insert-file-contents file)
         (buffer-string))
       "\n"))

    (defun +org/org--embed-css (file)
      "Convert css files to string for embedding"
      (concat
       "<style type=\"text/css\">\n"
       "<!--/*--><![CDATA[/*><!--*/\n"
       (with-temp-buffer
         (insert-file-contents file)
         (buffer-string))
       "/*]]>*/-->\n"
       "</style>\n"))

    (defun +org/org--embed-js (file)
      "Convert js files to string for embedding"
      (concat
       "<script type=\"text/javascript\">\n"
       "<!--/*--><![CDATA[/*><!--*/\n"
       (with-temp-buffer
         (insert-file-contents file)
         (buffer-string))
       "/*]]>*/-->\n"
       "</script>\n"))

    (defun +org/org--embed-header (filename)
      "Include file based on corresponding extensions"
      (let ((file (expand-file-name filename +org-html-export-style-dir)))
        (cond
         ((string-equal (file-name-extension file) "js")
          (+org/org--embed-js file))
         ((string-equal (file-name-extension file) "css")
          (+org/org--embed-css file))
         ((string-equal (file-name-extension file) "html")
          (+org/org--embed-html file)))))

    (defun +org/org-embed-header (exporter)
      "Insert custom inline css/scripts"
      (when (eq exporter 'html)
        (setq org-html-head
              (mapconcat #'+org/org--embed-header +org-html-export-style-alist "\n"))))

    (add-hook 'org-export-before-processing-hook #'+org/org-embed-header))

  ;; Export to a central location by default or if target isn't in `+org-dir'.
  (setq org-export-directory (expand-file-name ".export" +org-dir))
  (unless (file-directory-p org-export-directory)
    (make-directory org-export-directory t))

  (defun +org*export-output-file-name (args)
    "Return a centralized export location unless one is provided or the current
file isn't in `+org-dir'."
    (when (and (not (nth 2 args))
               buffer-file-name
               (file-in-directory-p (file-truename buffer-file-name) (file-truename +org-dir)))
      (setq args (append args (list org-export-directory))))
    args)
  (advice-add #'org-export-output-file-name :filter-args #'+org*export-output-file-name))
