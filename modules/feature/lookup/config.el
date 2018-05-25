;;; feature/lookup/config.el -*- lexical-binding: t; -*-

;; "What am I looking at?" This module helps you answer this question.
;;
;;   + `+lookup/definition': a jump-to-definition that should 'just work'
;;   + `+lookup/references': find a symbol's references in the current project
;;   + `+lookup/online'; look up a symbol on online resources
;;   + `+lookup/docs-at-point'
;;   + `+lookup/docs-dash'
;;   + `+lookup/docs-dash-at-point'
;;   + `+lookup/devdocs'
;;   + `+lookup/devdocs-at-point'
;;
;; This module uses `xref', an experimental new library in Emacs. It may change
;; in the future. When xref can't be depended on it will fall back to
;; `dumb-jump' to find what you want.

(defvar +lookup-provider-url-alist
  '(("Google"            . "https://google.com/search?q=%s")
    ("Google images"     . "https://google.com/images?q=%s")
    ("Google maps"       . "https://maps.google.com/maps?q=%s")
    ("Project Gutenberg" . "http://www.gutenberg.org/ebooks/search/?query=%s")
    ("DuckDuckGo"        . "https://duckduckgo.com/?q=%s")
    ("DevDocs.io"        . "https://devdocs.io/#q=%s")
    ("StackOverflow"     . "https://stackoverflow.com/search?q=%s")
    ("Github"            . "https://github.com/search?ref=simplesearch&q=%s")
    ("Youtube"           . "https://youtube.com/results?aq=f&oq=&search_query=%s")
    ("Wolfram alpha"     . "https://wolframalpha.com/input/?i=%s")
    ("Wikipedia"         . "https://wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"))
  "An alist that maps online resources to their search url or a function that
produces an url. Used by `+lookup/online'.")

(defvar +lookup-open-url-fn #'browse-url
  "Function to use to open search urls.")

(defvar +lookup-definition-functions '(+lookup-xref-definitions)
  "Functions for `+lookup/definition' to try, before resorting to `dumb-jump'.
Stops at the first function to return non-nil or change the current
window/point.

If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point.")

(defvar +lookup-references-functions '(+lookup-xref-references)
  "Functions for `+lookup/references' to try, before resorting to `dumb-jump'.
Stops at the first function to return non-nil or change the current
window/point.

If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point.")

(defvar +lookup-documentation-functions ()
  "Functions for `+lookup/documentation' to try, before resorting to
`dumb-jump'. Stops at the first function to return non-nil or change the current
window/point.

If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point.")

(defvar +lookup-file-functions ()
  "Function for `+lookup/file' to try, before restoring to `find-file-at-point'.
Stops at the first function to return non-nil or change the current
window/point.

If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point.")

(def-setting! :lookup (modes &rest plist)
  "Defines a jump target for major MODES. PLIST accepts the following
properties:

  :definition FN
    Run when jumping to a symbol's definition.
    Used by `+lookup/definition'.
  :references FN
    Run when looking for usage references of a symbol in the current project.
    Used by `+lookup/references'.
  :documentation FN
    Run when looking up documentation for a symbol.
    Used by `+lookup/documentation'.
  :file FN
    Run when looking up the file for a symbol/string. Typically a file path.
    Used by `+lookup/file'.
  :xref-backend FN
    Defines an xref backend for a major-mode. With this, :definition and
    :references are unnecessary.

Using this multiple times overwrites previous properties and unsets omitted
ones."
  `(progn
     ,@(cl-loop for mode in (doom-enlist (doom-unquote modes))
                for def-name = (intern (format "doom--init-lookup-%s" mode))
                collect
                `(defun ,def-name ()
                   (when (or (eq major-mode ',mode)
                             (bound-and-true-p ,mode))
                     (let ((xref ,(plist-get plist :xref-backend))
                           (def ,(plist-get plist :definition))
                           (ref ,(plist-get plist :references))
                           (fil ,(plist-get plist :file))
                           (doc ,(plist-get plist :documentation)))
                       (if xref (add-hook 'xref-backend-functions xref nil t))
                       (if def (add-hook '+lookup-definition-functions def nil t))
                       (if ref (add-hook '+lookup-references-functions ref nil t))
                       (if fil (add-hook '+lookup-file-functions fil nil t))
                       (if doc (add-hook '+lookup-documentation-functions doc nil t)))))
                collect `(add-hook! ,mode #',def-name))))

;; Recenter buffer after certain jumps
(add-hook!
  '(imenu-after-jump-hook evil-jumps-post-jump-hook
    counsel-grep-post-action-hook dumb-jump-after-jump-hook)
  #'recenter)


;;
;; dumb-jump
;;

(def-package! dumb-jump
  :commands dumb-jump-result-follow
  :config
  (setq dumb-jump-default-project doom-emacs-dir
        dumb-jump-aggressive nil
        dumb-jump-selector
        (cond ((featurep! :completion ivy)  'ivy)
              ((featurep! :completion helm) 'helm)
              ('popup))))


;;
;; xref
;;

;; By default, `etags--xref-backend' is the default xref backend. No need. We'll
;; set these up ourselves in other modules.
(setq-default xref-backend-functions '(t))

;; ...however, it breaks `projectile-find-tag', unless we put it back.
(defun +lookup*projectile-find-tag (orig-fn)
  (let ((xref-backend-functions '(etags--xref-backend t)))
    (funcall orig-fn)))
(advice-add #'projectile-find-tag :around #'+lookup*projectile-find-tag)


(def-package! ivy-xref
  :when (featurep! :completion ivy)
  :after xref
  :config (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))


(def-package! helm-xref
  :when (featurep! :completion helm)
  :after xref
  :config (setq xref-show-xrefs-function #'helm-xref-show-xrefs))


;;
;; Dash docset integration
;;

(when (featurep! +docsets)
  ;; Both packages depend on helm-dash
  (def-package! helm-dash
    :defer t
    :init
    (setq helm-dash-enable-debugging doom-debug-mode
          helm-dash-browser-func #'eww)
    :config
    (unless (file-directory-p helm-dash-docsets-path)
      (setq helm-dash-docsets-path (concat doom-etc-dir "docsets/")))
    (unless (file-directory-p helm-dash-docsets-path)
      (make-directory helm-dash-docsets-path t)))

  (def-package! counsel-dash
    :when (featurep! :completion ivy)
    :commands counsel-dash-install-docset
    :config (setq counsel-dash-min-length 2)))


;;
;; devdocs.io integration
;;

(when (featurep! +devdocs)
  (after! devdocs-lookup
    (unless (assoc "SCSS" devdocs-subjects)
      (setq devdocs-subjects
            (append '(("SCSS" "scss")
                      ("GFM" "markdown")
                      ("Typescript" "typescript"))
                    devdocs-subjects)))))

