;;; feature/jump/config.el -*- lexical-binding: t; -*-

;; "What am I looking at?"
;;
;; This module helps you answer that question. It helps you look up whatever
;; you're looking at.
;;
;;   + `+jump/definition': a jump-to-definition that should 'just work'
;;   + `+jump/references': find a symbol's references in the current project
;;   + `+jump/online'; look up a symbol on online resources, like stackoverflow,
;;     devdocs.io or google.
;;
;; This module uses `xref', an experimental new library in Emacs. It may change
;; in the future. When xref can't be depended on it will fall back to
;; `dumb-jump' to find what you want.

(defvar +jump-search-provider-alist
  '(("Google"        . "https://google.com/search?q=%s")
    ("DuckDuckGo"    . "https://duckduckgo.com/?q=%s")
    ("DevDocs.io"    . "http://devdocs.io/#q=%s")
    ("StackOverflow" . "https://stackoverflow.com/search?q=%s"))
  "An alist that maps online resources to their search url or a function that
produces an url. Used by `+jump/online'.")

(defvar +jump-function-alist nil
  "TODO")

(defvar-local +jump-current-functions nil
  "TODO")

(def-setting! :jump (modes &rest plist)
  "Definies a jump target for major MODES. PLIST accepts the following
properties:

  :definition FN
    Run when jumping to a symbol's definition.
    Used by `+jump/definition'.
  :references FN
    Run when looking for usage references of a symbol in the current project.
    Used by `+jump/references'.
  :documentation FN
    Run when looking up documentation for a symbol.
    Used by `+jump/documentation'."
  `(dolist (mode (doom-enlist ,modes))
     (push (cons mode (list ,@plist)) +jump-function-alist)))

;; Let me control what backends to fall back on
(setq-default xref-backend-functions '(t))

(set! :popup "*xref*" :noselect t :autokill t :autoclose t)

;; Recenter after certain jumps
(add-hook!
  '(imenu-after-jump-hook evil-jumps-post-jump-hook
    counsel-grep-post-action-hook dumb-jump-after-jump-hook)
  #'recenter)

(defun +jump|init ()
  "Initialize `+jump-current-functions', used by `+jump/definition',
`+jump/references' and `+jump/documentation'."
  (when-let (plist (cdr (assq major-mode +jump-function-alist)))
    (when-let (backend (plist-get plist :xref-backend))
      (make-variable-buffer-local 'xref-backend-functions)
      (cl-pushnew backend xref-backend-functions :test #'eq))
    (setq-local +jump-current-functions plist)))
(add-hook 'after-change-major-mode-hook #'+jump|init)


;;
;; Packages
;;

(def-package! dumb-jump
  :commands (dumb-jump-go dumb-jump-quick-look
             dumb-jump-back dumb-jump-result-follow)
  :config
  (setq dumb-jump-default-project doom-emacs-dir
        dumb-jump-aggressive nil
        dumb-jump-selector (cond ((featurep! :completion ivy) 'ivy)
                                 ((featurep! :completion helm) 'helm)
                                 (t 'popup))))


(def-package! gxref
  :commands (gxref-xref-backend
             gxref-create-db
             gxref-update-db
             gxref-single-update-db
             gxref-set-project-dir)
  :init
  (setq-default xref-backend-functions '(gxref-xref-backend t)))


;; (def-package! ggtags
;;   :commands (ggtags-find-tag-dwim
;;              ggtags-find-tag-mouse
;;              ggtags-find-definition
;;              ggtags-find-reference
;;              ggtags-find-other-symbol
;;              ggtags-find-tag-regexp
;;              ggtags-idutils-query
;;              ggtags-grep
;;              ggtags-find-file
;;              ggtags-query-replace
;;              ggtags-delete-tags
;;              ggtags-explain-tags))

