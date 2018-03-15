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

(defvar +lookup-function-alist nil
  "An alist mapping major modes to jump function plists, describing what to do
with `+lookup/definition', `+lookup/references' and `+lookup/documentation' are
called.")

(defvar-local +lookup-current-functions nil
  "The enabled jump functions for the current buffer.")

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
  :xref-backend FN
    Defines an xref backend for a major-mode. With this, :definition and
    :references are unnecessary."
  `(dolist (mode (doom-enlist ,modes))
     (push (cons mode (list ,@plist))
           +lookup-function-alist)))

;; Recenter buffer after certain jumps
(add-hook!
  '(imenu-after-jump-hook evil-jumps-post-jump-hook
    counsel-grep-post-action-hook dumb-jump-after-jump-hook)
  #'recenter)


;;
;; dumb-jump
;;

(def-package! dumb-jump
  :commands (dumb-jump-go dumb-jump-quick-look
             dumb-jump-back dumb-jump-result-follow)
  :config
  (setq dumb-jump-default-project doom-emacs-dir
        dumb-jump-aggressive nil
        dumb-jump-selector
        (cond ((featurep! :completion ivy)  'ivy)
              ((featurep! :completion helm) 'helm)
              (t 'popup))))


;;
;; xref
;;

(after! xref
  ;; By default, `etags--xref-backend' is the default xref backend. No need.
  ;; We'll set these up ourselves in other modules.
  (setq-default xref-backend-functions '(t))

  ;; ...however, it breaks `projectile-find-tag', unless we put it back.
  (defun +lookup*projectile-find-tag (orig-fn)
    (let ((xref-backend-functions '(etags--xref-backend t)))
      (funcall orig-fn)))
  (advice-add #'projectile-find-tag :around #'+lookup*projectile-find-tag))

(defun +lookup|init-xref-backends ()
  "Set `+lookup-current-functions' for the current buffer.

This variable is used by `+lookup/definition',`+lookup/references' and
`+lookup/documentation'."
  (when-let* ((plist (cdr (assq major-mode +lookup-function-alist))))
    (when-let* ((backend (plist-get plist :xref-backend)))
      (make-variable-buffer-local 'xref-backend-functions)
      (cl-pushnew backend xref-backend-functions :test #'eq))
    (setq-local +lookup-current-functions plist)))
(add-hook 'after-change-major-mode-hook #'+lookup|init-xref-backends)


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
  (def-setting! :docset (modes &rest docsets)
    "Registers a list of DOCSETS (strings) for MODES (either one major mode
symbol or a list of them).

If MODES is a minor mode, you can use :add or :remove as the first element of
DOCSETS, to instruct it to append (or remove) those from the docsets already set
by a major-mode, if any.

Used by `+lookup/in-docsets' and `+lookup/documentation'."
    (let* ((modes (doom-unquote modes))
           (ivy-p (featurep! :completion ivy))
           (hook-sym (intern (format "+lookup|%s-docsets--%s"
                                     (cond ((eq ',(car docsets) :add)    'add)
                                           ((eq ',(car docsets) :remove) 'remove)
                                           ('set))
                                     (string-join docsets "-"))))
           (var-sym (if ivy-p 'counsel-dash-docsets 'helm-dash-docsets)))
      `(progn
         (defun ,hook-sym ()
           (make-variable-buffer-local ',var-sym)
           ,(cond ((eq ',(car docsets) :add)
                   `(setq ,var-sym (append ,var-sym (list ,@(cdr docsets)))))
                  ((eq ',(car docsets) :remove)
                   `(setq ,var-sym
                          (cl-loop with to-delete = (list ,@(cdr docsets))
                                   for docset in ,var-sym
                                   unless (member docset to-delete)
                                   collect docset)))
                  (`(setq ,var-sym (list ,@docsets)))))
         (add-hook! ,modes #',hook-sym))))

  ;; Both packages depend on helm-dash
  (def-package! helm-dash
    :commands (helm-dash helm-dash-install-docset helm-dash-at-point
               helm-dash-docset-installed-p helm-dash-installed-docsets)
    :config
    (unless (file-directory-p helm-dash-docsets-path)
      (setq helm-dash-docsets-path (concat doom-etc-dir "docsets/")))
    (unless (file-directory-p helm-dash-docsets-path)
      (make-directory helm-dash-docsets-path t))
    (setq helm-dash-enable-debugging doom-debug-mode))

  (def-package! counsel-dash
    :when (featurep! :completion ivy)
    :commands (counsel-dash counsel-dash-install-docset)
    :after helm-dash
    :config (setq counsel-dash-min-length 2)))


;;
;; devdocs.io integration
;;

(when (featurep! +devdocs)
  (def-setting! :devdocs (modes docset)
    "Map major MODES (one major-mode symbol or a list of them) to a devdocs
DOCSET (a string).

See `devdocs-alist' for the defaults. "
    `(dolist (mode ',modes)
       (push (cons mode ,docset) devdocs-alist)))

  (def-package! devdocs
    :defer t
    :config
    (setq devdocs-alist
          (append '((rust-mode . "rust")
                    (scss-mode . "scss")
                    (gfm-mode . "markdown")
                    (nim-mode . "nim")
                    (typescript-mode . "typescript"))
                  devdocs-alist))))

