;; NARF Dark
;; By Henrik Lissner <http://github.com/hlissner/emacs.d>

(deftheme narf-dark "dark theme for narfy emacs")

(custom-theme-set-variables 'narf-dark)

(let ((background       "#2b303b")
      (gutters          "#1f252a")
      (gutter-fg        "#55616A")
      (gutters-active   "#1c1f26")
      (linum            "#1e262c")
      (gutter-light     "#232830")
      (builtin          "#d08770")
      (foreground       "#c0c5ce")
      (invisibles       "#65737e")
      (line-hl          "#343d46")
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
      (git-deleted      "#714243"))

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

   `(mode-line                ((t (:foreground ,white
                                           :background ,gutter-light
                                           :box (:line-width 3 :color ,gutter-light)
                                           ))))

   `(mode-line-inactive       ((t (:foreground ,gutter-fg
                                           :background ,gutters-active
                                           :box (:line-width 3 :color ,gutters-active)
                                           ))))

   `(mode-line-modified-face  ((t (:foreground ,builtin))))

   `(sml/folder               ((t nil)))
   `(sml/modified             ((t (:foreground ,highlight))))

   `(flyspell-incorrect       ((t (:underline "#ff5555" :inherit unspecified))))

   `(helm-source-header       ((t (:background ,gutters-active :foreground ,strings :weight bold :height 1.0))))
   `(helm-selection           ((t (:background ,selection))))

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
   `(whitespace-tab                    ((t (:foreground ,line-hl))))
   `(whitespace-newline                ((t (:foreground ,line-hl))))
   `(whitespace-trailing               ((t (:background "#553333"))))

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

   ;; js2-mode
   ;; *****************************************************************************************

   `(js2-function-param                ((t (:foreground ,variables))))
   `(js2-jsdoc-tag                     ((t (:foreground ,comments :weight bold :bold t))))

   ;; company-mode
   ;; *****************************************************************************************

   `(company-tooltip                   ((t (:inherit default :background "#3e4555"))))
   `(company-tooltip-selection         ((t (:inherit font-lock-function-name-face))))
   `(company-tooltip-common            ((t (:inherit font-lock-constant-face))))
   `(company-scrollbar-bg              ((t (:background "#4b5367"))))
   `(company-scrollbar-fg              ((t (:background "#353b49"))))
   `(company-search                    ((t (:background "#4b5367"))))

   ;; *****************************************************************************************

   `(persp-selected-face               ((t (:foreground ,builtin))))

   `(org-level-1                      ((t (:inherit outline-1 :bold t :foreground ,constants))))
   `(org-level-2                      ((t (:inherit outline-2 :bold t :foreground ,variables))))

   `(show-paren-match                 ((t (:background nil :foreground ,highlight :weight ultra-bold))))

   `(evil-snipe-first-match-face      ((t (:background ,highlight :foreground "black"))))
   `(evil-snipe-matches-face          ((t (:foreground ,highlight :background ,dim-highlight))))
   `(isearch                          ((t (:foreground "black" :background ,highlight :inverse-video nil))))
   `(isearch-lazy-highlight-face      ((t (:foreground ,text :background ,dim-highlight))))
   `(evil-search-highlight-persist-highlight-face ((t (:inherit isearch-lazy-highlight-face))))

   ))


;; *****************************************************************************************

(provide-theme 'narf-dark)

;; Local Variables:
;; no-byte-compile: t
;; End:
