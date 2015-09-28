;; NARF Dark
;; By Henrik Lissner <http://github.com/hlissner/emacs.d>

(deftheme narf-dark "dark theme for narfy emacs")

(custom-theme-set-variables 'narf-dark)

(let ((background       "#232837")
      (gutters          "#05051e")
      (gutter-fg        "#55616A")
      (gutters-active   "#2b303f")
      (gutter-light     "#191e28")
      (linum            "#1e262c")
      (builtin          "#d08770")
      (foreground       "#c0c5ce")
      (invisibles       "#65737e")
      (line-hl          "#2b303f")
      (selection        "#4f5b66")
      (text             "#c0c5ce")
      (comments         "#65737e")
      (punctuation      "#8fa1b3")
      (delimiters       "#c0c5ce")
      (operators        "#c0c5ce")
      (keywords         "#b48ead")
      (variables        "#CBECFF")
      (functions        "#8fa1b3")
      (methods          "#8fa1b3")
      (strings          "#a3be8c")
      (constants        "#d08770")
      (white            "#ffffff")
      (highlight        "orange")
      (dim-highlight    "#556779")

      (git-modified     "#55616A")
      (git-added        "#436b3b")
      (git-deleted      "#714243")

      (dark-bg          "#05051e"))

  (custom-theme-set-faces
   'narf-dark

   ;; Default colors
   `(default                  ((t (:foreground ,text :background ,background) )))
   `(hl-line                  ((t (:background ,line-hl) )))
   `(region                   ((t (:background ,selection) )))
   `(cursor                   ((t (:background ,white) )))
   `(fringe                   ((t (:background ,background :foreground ,white) )))
   `(linum                    ((t (:background ,background :foreground ,gutter-fg :weight normal) )))

   `(vertical-border          ((t (:foreground "#000000") )))

   `(mode-line                ((t (:foreground ,white     :background ,gutter-light))))
   `(mode-line-inactive       ((t (:foreground ,gutter-fg :background ,gutters-active))))
   `(mode-line-modified-face  ((t (:foreground ,builtin))))
   `(powerline-active1        ((t (:background "#343A4D"))))
   `(powerline-active2        ((t (:background "#3B435C"))))
   `(powerline-inactive1      ((t (:background ,gutters-active))))
   `(powerline-inactive2      ((t (:background ,gutters-active))))

   ;; Font lock faces
   `(linum-highlight-face              ((t (:foreground ,text :background ,line-hl :inherit linum))))

   `(font-lock-keyword-face            ((t (:foreground ,keywords))))
   `(font-lock-type-face               ((t (:foreground ,punctuation))))
   `(font-lock-constant-face           ((t (:foreground ,constants))))
   `(font-lock-variable-name-face      ((t (:foreground ,variables))))
   `(font-lock-builtin-face            ((t (:foreground ,builtin))))
   `(font-lock-string-face             ((t (:foreground ,strings))))
   `(font-lock-comment-face            ((t (:foreground ,comments))))
   `(font-lock-comment-delimiter-face  ((t (:foreground ,comments))))
   `(font-lock-function-name-face      ((t (:foreground ,functions))))
   `(font-lock-doc-string-face         ((t (:foreground ,comments))))
   `(font-lock-doc-face                ((t (:foreground ,comments))))

   `(trailing-whitespace               ((t (:background "#884444"))))
   `(whitespace-tab                    ((t (:foreground "#343d46"))))
   `(whitespace-newline                ((t (:foreground "#343d46"))))
   `(whitespace-trailing               ((t (:background "#553333"))))

   ;; lang-specific
   ;; *****************************************************************************************

   `(js2-function-param                ((t (:foreground ,variables))))
   `(js2-jsdoc-tag                     ((t (:foreground ,comments :weight bold :bold t))))

   `(org-level-1                      ((t (:inherit outline-1 :bold t :foreground ,constants))))
   `(org-level-2                      ((t (:inherit outline-2 :bold t :foreground ,variables))))

   ;; company-mode
   ;; *****************************************************************************************

   `(company-tooltip                   ((t (:inherit default :background "#3e4555"))))
   `(company-tooltip-selection         ((t (:inherit font-lock-function-name-face))))
   `(company-tooltip-common            ((t (:inherit font-lock-constant-face))))
   `(company-scrollbar-bg              ((t (:background "#4b5367"))))
   `(company-scrollbar-fg              ((t (:background "#353b49"))))
   `(company-search                    ((t (:background "#4b5367"))))

   ;; *****************************************************************************************

   `(flyspell-incorrect       ((t (:underline "#ff5555" :inherit unspecified))))

   `(helm-source-header       ((t (:background ,gutters-active :foreground ,strings :weight bold :height 1.0))))
   `(helm-selection           ((t (:background ,selection))))

   `(highlight-indentation-face                 ((t (:background "#2f3641"))))
   `(highlight-indentation-current-column-face  ((t (:background ,gutter-light))))

   `(git-gutter+-modified              ((t (:foreground ,git-modified :background nil))))
   `(git-gutter+-added                 ((t (:foreground ,git-added :background nil))))
   `(git-gutter+-deleted               ((t (:foreground ,git-deleted :background nil))))

   `(diff-hl-change                    ((t (:background ,git-modified))))
   `(diff-hl-delete                    ((t (:background ,git-deleted))))
   `(diff-hl-insert                    ((t (:background ,git-added))))

   `(rainbow-delimiters-unmatched-face ((t (:inherit 'error))))
   `(rainbow-delimiters-depth-1-face   ((t (:foreground "#CCCCCC" :weight bold :bold t))))

   `(show-paren-match                 ((t (:background nil :foreground ,highlight :weight ultra-bold))))

   `(evil-snipe-first-match-face      ((t (:background ,highlight :foreground "black"))))
   `(evil-snipe-matches-face          ((t (:foreground ,highlight :background ,dim-highlight))))
   `(evil-search-highlight-persist-highlight-face ((t (:inherit isearch-lazy-highlight-face))))
   `(isearch                          ((t (:foreground "black" :background ,highlight :inverse-video nil))))
   `(isearch-lazy-highlight-face      ((t (:foreground ,text :background ,dim-highlight))))

   ))


;; *****************************************************************************************

(provide-theme 'narf-dark)

;; Local Variables:
;; no-byte-compile: t
;; End:
