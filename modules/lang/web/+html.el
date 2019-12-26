;;; lang/web/+html.el -*- lexical-binding: t; -*-

(use-package! web-mode
  ;; REVIEW We associate TSX files with `web-mode' because `typescript-mode'
  ;;        does not officially support JSX/TSX. See
  ;;        https://github.com/emacs-typescript/typescript.el/issues/4
  :mode "\\.\\(?:as\\(?:[cp]x\\)\\|blade\\.php\\|erb\\|hbs\\|j\\(?:inja\\|sp\\)\\|mustache\\|p?html?\\|svelte\\|t\\(?:pl\\.php\\|sx\\|wig\\)\\|vue\\)\\'"
  :mode "wp-content/themes/.+/.+\\.php\\'"
  :mode "templates/.+\\.php\\'"
  :config
  (set-docsets! 'web-mode "HTML" "CSS" "Twig" "WordPress")

  ;; tidy is already defined by the format-all package. We redefine it to add
  ;; more sensible arguments to the tidy command.
  (set-formatter! 'html-tidy
    '("tidy" "-q" "-indent"
      "--tidy-mark" "no"
      "--drop-empty-elements" "no"
      "--show-body-only" "auto"  ; don't inject html/body tags
      ("--indent-spaces" "%d" tab-width)
      ("--indent-with-tabs" "%s" (if indent-tabs-mode "yes" "no"))
      ("-xml" (memq major-mode '(nxml-mode xml-mode))))
    :ok-statuses '(0 1))

  (setq web-mode-enable-html-entities-fontification t
        web-mode-auto-close-style 1)

  (after! smartparens
    (defun +web-is-auto-close-style-3 (_id action _context)
      (and (eq action 'insert)
           (eq web-mode-auto-close-style 3)))
    (sp-local-pair 'web-mode "<" ">" :unless '(:add +web-is-auto-close-style-3))

    ;; let smartparens handle these
    (setq web-mode-enable-auto-quoting nil
          web-mode-enable-auto-pairing t)

    ;; 1. Remove web-mode auto pairs whose end pair starts with a latter
    ;;    (truncated autopairs like <?p and hp ?>). Smartparens handles these
    ;;    better.
    ;; 2. Strips out extra closing pairs to prevent redundant characters
    ;;    inserted by smartparens.
    (dolist (alist web-mode-engines-auto-pairs)
      (setcdr alist
              (cl-loop for pair in (cdr alist)
                       unless (string-match-p "^[a-z-]" (cdr pair))
                       collect (cons (car pair)
                                     (string-trim-right (cdr pair)
                                                        "\\(?:>\\|]\\|}\\)+\\'")))))
    (delq! nil web-mode-engines-auto-pairs))

  (map! :map web-mode-map
        (:localleader
          :desc "Rehighlight buffer" "h" #'web-mode-buffer-highlight
          :desc "Indent buffer"      "i" #'web-mode-buffer-indent
          (:prefix ("a" . "attribute")
            "b" #'web-mode-attribute-beginning
            "e" #'web-mode-attribute-end
            "i" #'web-mode-attribute-insert
            "n" #'web-mode-attribute-next
            "s" #'web-mode-attribute-select
            "k" #'web-mode-attribute-kill
            "p" #'web-mode-attribute-previous
            "p" #'web-mode-attribute-transpose)
          (:prefix ("b" . "block")
            "b" #'web-mode-block-beginning
            "c" #'web-mode-block-close
            "e" #'web-mode-block-end
            "k" #'web-mode-block-kill
            "n" #'web-mode-block-next
            "p" #'web-mode-block-previous
            "s" #'web-mode-block-select)
          (:prefix ("d" . "dom")
            "a" #'web-mode-dom-apostrophes-replace
            "d" #'web-mode-dom-errors-show
            "e" #'web-mode-dom-entities-encode
            "n" #'web-mode-dom-normalize
            "q" #'web-mode-dom-quotes-replace
            "t" #'web-mode-dom-traverse
            "x" #'web-mode-dom-xpath)
          (:prefix ("e" . "element")
            "/" #'web-mode-element-close
            "a" #'web-mode-element-content-select
            "b" #'web-mode-element-beginning
            "c" #'web-mode-element-clone
            "d" #'web-mode-element-child
            "e" #'web-mode-element-end
            "f" #'web-mode-element-children-fold-or-unfold
            "i" #'web-mode-element-insert
            "k" #'web-mode-element-kill
            "m" #'web-mode-element-mute-blanks
            "n" #'web-mode-element-next
            "p" #'web-mode-element-previous
            "r" #'web-mode-element-rename
            "s" #'web-mode-element-select
            "t" #'web-mode-element-transpose
            "u" #'web-mode-element-parent
            "v" #'web-mode-element-vanish
            "w" #'web-mode-element-wrap)
          (:prefix ("t" . "tag")
            "a" #'web-mode-tag-attributes-sort
            "b" #'web-mode-tag-beginning
            "e" #'web-mode-tag-end
            "m" #'web-mode-tag-match
            "n" #'web-mode-tag-next
            "p" #'web-mode-tag-previous
            "s" #'web-mode-tag-select))

        :g  "M-/" #'web-mode-comment-or-uncomment
        :i  "SPC" #'self-insert-command
        :n  "za"  #'web-mode-fold-or-unfold
        :nv "]a"  #'web-mode-attribute-next
        :nv "[a"  #'web-mode-attribute-previous
        :nv "]t"  #'web-mode-tag-next
        :nv "[t"  #'web-mode-tag-previous
        :nv "]T"  #'web-mode-element-child
        :nv "[T"  #'web-mode-element-parent))


;;
(after! pug-mode
  (set-company-backend! 'pug-mode 'company-web-jade))
(after! web-mode
  (set-company-backend! 'web-mode 'company-css 'company-web-html))
(after! slim-mode
  (set-company-backend! 'slim-mode 'company-web-slim))


(when (featurep! +lsp)
  (add-hook! '(html-mode-hook web-mode-hook) #'lsp!))
