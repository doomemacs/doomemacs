;;; lang/web/+html.el -*- lexical-binding: t; -*-

(def-package! web-mode
  :mode "\\.p?html?$"
  :mode "\\.\\(?:tpl\\|blade\\)\\(\\.php\\)?$"
  :mode "\\.erb$"
  :mode "\\.jsp$"
  :mode "\\.as[cp]x$"
  :mode "\\.hbs$"
  :mode "\\.mustache$"
  :mode "\\.tsx$"
  :mode "\\.vue$"
  :mode "\\.twig$"
  :mode "\\.jinja$"
  :mode "wp-content/themes/.+/.+\\.php$"
  :mode "templates/.+\\.php$"
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
        web-mode-auto-close-style 2)

  (after! smartparens
    (defun +web-is-auto-close-style-3 (_id action _context)
      (and (eq action 'insert)
           (eq web-mode-auto-close-style 3)))
    (sp-local-pair 'web-mode "<" nil :unless '(:add +web-is-auto-close-style-3))

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
                                     ;; TODO Replace with `string-trim-right' (Emacs 26+)
                                     (let ((string (cdr pair)))
                                       (if (string-match "\\(?:>\\|]\\|}\\)+\\'" string)
                                           (replace-match "" t t string)
                                         string))))))
    (setf (alist-get nil web-mode-engines-auto-pairs) nil))

  (map! :map web-mode-map
        (:localleader
          :desc "Rehighlight buffer" :n "h" #'web-mode-buffer-highlight
          :desc "Indent buffer"      :n "i" #'web-mode-buffer-indent

          (:desc "attribute" :prefix "a"
            :desc "Beginning" :n "b" #'web-mode-attribute-beginning
            :desc "End"       :n "e" #'web-mode-attribute-end
            :desc "Insert"    :n "i" #'web-mode-attribute-insert
            :desc "Next"      :n "n" #'web-mode-attribute-next
            :desc "Select"    :n "s" #'web-mode-attribute-select
            :desc "Kill"      :n "k" #'web-mode-attribute-kill
            :desc "Previous"  :n "p" #'web-mode-attribute-previous
            :desc "Transpose" :n "p" #'web-mode-attribute-transpose)

          (:desc "block" :prefix "b"
            :desc "Beginning" :n "b" #'web-mode-block-beginning
            :desc "Close"     :n "c" #'web-mode-block-close
            :desc "End"       :n "e" #'web-mode-block-end
            :desc "Kill"      :n "k" #'web-mode-block-kill
            :desc "Next"      :n "n" #'web-mode-block-next
            :desc "Previous"  :n "p" #'web-mode-block-previous
            :desc "Select"    :n "s" #'web-mode-block-select)

          (:desc "dom" :prefix "d"
            :desc "Replace apostrophes" :n "a" #'web-mode-dom-apostrophes-replace
            :desc "Show errors"         :n "d" #'web-mode-dom-errors-show
            :desc "Replace entities"    :n "e" #'web-mode-dom-entities-encode
            :desc "Normalize"           :n "n" #'web-mode-dom-normalize
            :desc "Replace quotes"      :n "q" #'web-mode-dom-quotes-replace
            :desc "Traverse"            :n "t" #'web-mode-dom-traverse
            :desc "XPath"               :n "x" #'web-mode-dom-xpath)

          (:desc "element" :prefix "e"
            :desc "Close"          :n "/" #'web-mode-element-close
            :desc "Select content" :n "a" #'web-mode-element-content-select
            :desc "Beginning"      :n "b" #'web-mode-element-beginning
            :desc "Close"          :n "c" #'web-mode-element-clone
            :desc "Child"          :n "d" #'web-mode-element-child
            :desc "End"            :n "e" #'web-mode-element-end
            :desc "Toggle fold"    :n "f" #'web-mode-element-children-fold-or-unfold
            :desc "Insert"         :n "i" #'web-mode-element-insert
            :desc "Kill"           :n "k" #'web-mode-element-kill
            :desc "Mute blanks"    :n "m" #'web-mode-element-mute-blanks
            :desc "Next"           :n "n" #'web-mode-element-next
            :desc "Previous"       :n "p" #'web-mode-element-previous
            :desc "Rename"         :n "r" #'web-mode-element-rename
            :desc "Select"         :n "s" #'web-mode-element-select
            :desc "Transpose"      :n "t" #'web-mode-element-transpose
            :desc "Parent"         :n "u" #'web-mode-element-parent
            :desc "Vanish"         :n "v" #'web-mode-element-vanish
            :desc "Wrap"           :n "w" #'web-mode-element-wrap)

          (:desc "tag" :prefix "t"
            :desc "Sort attributes" :n "a" #'web-mode-tag-attributes-sort
            :desc "Beginning"       :n "b" #'web-mode-tag-beginning
            :desc "End"             :n "e" #'web-mode-tag-end
            :desc "Match"           :n "m" #'web-mode-tag-match
            :desc "Next"            :n "n" #'web-mode-tag-next
            :desc "Previous"        :n "p" #'web-mode-tag-previous
            :desc "Select"          :n "s" #'web-mode-tag-select))

        "M-/" #'web-mode-comment-or-uncomment
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
  (set-company-backend! 'web-mode 'company-web-html))
(after! slim-mode
  (set-company-backend! 'slim-mode 'company-web-slim))
