;;; lang/markdown/config.el -*- lexical-binding: t; -*-

(defvar +markdown-compile-functions
  '(+markdown-compile-marked
    +markdown-compile-pandoc
    +markdown-compile-markdown
    +markdown-compile-multimarkdown)
  "A list of commands to try when attempting to build a markdown file with
`markdown-open' or `markdown-preview', stopping at the first one to return non-nil.

Each function takes three argument. The beginning position of the region to
capture, the end position, and the output buffer.")


;;
;;; Packages

(use-package! markdown-mode
  :mode ("/README\\(?:\\.md\\)?\\'" . gfm-mode)
  :init
  (setq markdown-enable-math t ; syntax highlighting for latex fragments
        markdown-enable-wiki-links t
        markdown-italic-underscore t
        markdown-asymmetric-header t
        markdown-fontify-code-blocks-natively t
        markdown-gfm-uppercase-checkbox t ; for compat with org-mode
        markdown-gfm-additional-languages '("sh")
        markdown-make-gfm-checkboxes-buttons t

        ;; Preview/compilation defaults
        markdown-command #'+markdown-compile
        markdown-open-command
        (cond (IS-MAC "open")
              (IS-LINUX "xdg-open"))
        markdown-content-type "application/xhtml+xml"
        markdown-css-paths
        '("https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown.min.css"
          "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/styles/github.min.css")
        markdown-xhtml-header-content
        (concat "<meta name='viewport' content='width=device-width, initial-scale=1, shrink-to-fit=no'>"
                "<style> body { box-sizing: border-box; max-width: 740px; width: 100%; margin: 40px auto; padding: 0 10px; } </style>"
                "<script src='https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/highlight.min.js'></script>"
                "<script>document.addEventListener('DOMContentLoaded', () => { document.body.classList.add('markdown-body'); document.querySelectorAll('pre[lang] > code').forEach((code) => { code.classList.add(code.parentElement.lang); }); document.querySelectorAll('pre > code').forEach((code) => { hljs.highlightBlock(code); }); });</script>"))

  :config
  (set-flyspell-predicate! '(markdown-mode gfm-mode)
    #'+markdown-flyspell-word-p)
  (set-lookup-handlers! '(markdown-mode gfm-mode)
    ;; `markdown-follow-thing-at-point' may open an external program or a
    ;; buffer. No good way to tell, so pretend it's async.
    :file '(markdown-follow-thing-at-point :async t))

  (sp-local-pair '(markdown-mode gfm-mode) "`" "`"
                 :unless '(:add sp-point-before-word-p sp-point-before-same-p))

  (setq-hook! 'markdown-mode-hook
    fill-nobreak-predicate (cons #'markdown-code-block-at-point-p
                                 fill-nobreak-predicate))

  ;; HACK Prevent mis-fontification of YAML metadata blocks in `markdown-mode'
  ;;      which occurs when the first line contains a colon in it. See
  ;;      https://github.com/jrblevin/markdown-mode/issues/328.
  (defadvice! +markdown-disable-front-matter-fontification-a (&rest _)
    :override #'markdown-match-generic-metadata
    (ignore (goto-char (point-max))))

  (map! :map markdown-mode-map
        :localleader
        "o" #'markdown-open
        "p" #'markdown-preview
        "e" #'markdown-export
        (:when (featurep! +grip)
          "p" #'grip-mode)
        (:prefix ("i" . "insert")
          "t" #'markdown-toc-generate-toc
          "i" #'markdown-insert-image
          "l" #'markdown-insert-link)))


(use-package! evil-markdown
  :when (featurep! :editor evil +everywhere)
  :hook (markdown-mode . evil-markdown-mode)
  :init
  ;; REVIEW Until Somelauw/evil-markdown#1 is resolved:
  (defun evil-disable-insert-state-bindings ()
    evil-disable-insert-state-bindings)
  :config
  (add-hook 'evil-markdown-mode-hook #'evil-normalize-keymaps)
  (map! :map evil-markdown-mode-map
        :n "TAB" #'markdown-cycle
        :n [backtab] #'markdown-shifttab
        :i "M-*" #'markdown-insert-list-item
        :i "M-b" #'markdown-insert-bold
        :i "M-i" #'markdown-insert-italic
        :i "M-`" #'+markdown/insert-del
        :i "M--" #'markdown-insert-hr
        :n "M-r" #'browse-url-of-file
        :m "]h"  #'markdown-next-visible-heading
        :m "[h"  #'markdown-previous-visible-heading
        :m "[p"  #'markdown-promote
        :m "]p"  #'markdown-demote
        :m "[l"  #'markdown-previous-link
        :m "]l"  #'markdown-next-link))
