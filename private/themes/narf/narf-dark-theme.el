;; NARF Dark
;; By Henrik Lissner <http://github.com/hlissner/emacs.d>

(deftheme narf-dark "dark theme for narfy emacs")

(custom-theme-set-variables 'narf-dark)

(let* ((class '((class color) (min-colors 89)))

       (background       "#222226")
       (foreground       "#cccada")
       (subtle           "#aab6c7")
       (vsubtle          "#556172")
       (vvsubtle         "#354152")
       (highlight        "orange")
       (error-highlight  "#55ffff")

       (vertical-bar     "#000000")
       (current-line     "#232D3A")
       (selection        "#445f7f")
       (search           "#ccccff")
       (search-fg        "#000000")
       (search-rest      "#545060")
       (builtin          "#d08770")
       (comments         "#55626e")
       (constants        "#d08770")
       (delimiters       "#c0c5ce")
       (functions        "#8fa1b3")
       (keywords         "#b48ead")
       (methods          "#8fa1b3")
       (operators        "#c0c5ce")
       (punctuation      "#8fa1b3")
       (strings          "#a3be8c")
       (variables        "#CBECFF")

       (linum-bg         "#3B4750")
       (linum-fg         background)
       (linum-highlight  subtle)

       (modeline-bg-main      "#171c26")
       (modeline-bg-light     "#3D454E")
       (modeline-bg-inactive  "#111111")

       (white     "#ffffff")
       (off-white "#eaeadb")
       (yellow    "#b58900")
       (orange    "#cb4b16")
       (red       "#dc322f")
       (magenta   "#d33682")
       (violet    "#6c71c4")
       (blue      "#268bd2")
       (cyan      "#2aa198")
       (green     "#859900")

       (vc-modified      "#55616A")
       (vc-added         "#437f4b")
       (vc-deleted       "#864253")

       (dim-highlight    "#3f4b56"))

  (custom-theme-set-faces
   'narf-dark

   ;; Text
   `(default                  ((,class (:foreground ,foreground :background ,background) )))
   `(fringe                   ((,class (:background ,background :foreground ,foreground) )))
   `(cursor                   ((,class (:background ,white) )))
   `(hl-line                  ((,class (:background ,current-line) )))
   `(region                   ((,class (:background ,selection) )))

   `(font-lock-builtin-face            ((,class (:foreground ,builtin))))
   `(font-lock-comment-delimiter-face  ((,class (:foreground ,comments))))
   `(font-lock-comment-face            ((,class (:foreground ,comments))))
   `(font-lock-doc-face                ((,class (:foreground ,comments))))
   `(font-lock-doc-string-face         ((,class (:foreground ,comments))))
   `(font-lock-constant-face           ((,class (:foreground ,constants))))
   `(font-lock-function-name-face      ((,class (:foreground ,functions))))
   `(font-lock-keyword-face            ((,class (:foreground ,keywords))))
   ;; `(font-lock-negation-char-face      ((,class ())))
   ;; `(font-lock-preprocessor-char-face      ((,class ())))
   ;; `(font-lock-regexp-grouping-backslash      ((,class ())))
   ;; `(font-lock-regexp-grouping-construct      ((,class ())))
   `(font-lock-string-face             ((,class (:foreground ,strings))))
   `(font-lock-type-face               ((,class (:foreground ,punctuation))))
   `(font-lock-variable-name-face      ((,class (:foreground ,variables))))
   ;; `(font-lock-warning-face      ((,class ())))

   `(trailing-whitespace               ((t (:background "#884444"))))
   `(whitespace-tab                    ((t (:foreground "#343d46"))))
   `(whitespace-newline                ((t (:foreground "#343d46"))))
   `(whitespace-trailing               ((t (:background "#553333"))))

   ;; GUI
   `(vertical-border           ((,class (:foreground ,vertical-bar :background ,vertical-bar) )))

   `(linum                     ((,class (:background ,linum-fg     :foreground ,linum-bg        :bold nil) )))
   `(linum-highlight-face      ((,class (:inherit linum :background ,current-line :foreground ,linum-highlight))))
   `(show-paren-match          ((,class (:foreground ,highlight :weight ultra-bold))))

   `(mode-line                 ((,class (:foreground ,subtle :background ,modeline-bg-light))))
   `(powerline-active1         ((,class (:foreground ,subtle :background ,modeline-bg-light))))
   `(powerline-active2         ((,class (:foreground ,subtle :background ,modeline-bg-light))))
   `(mode-line-is-modified     ((,class (:foreground ,highlight))))
   `(mode-line-buffer-file     ((,class (:foreground ,off-white))))
   `(mode-line-buffer-dir      ((,class (:foreground ,subtle))))

   `(mode-line-inactive        ((,class (:foreground ,vsubtle :background ,modeline-bg-inactive))))
   `(powerline-inactive1       ((,class (:foreground ,vsubtle :background ,modeline-bg-inactive))))
   `(powerline-inactive2       ((,class (:foreground ,vsubtle :background ,modeline-bg-inactive))))

   ;; company-mode
   `(company-tooltip                   ((,class (:inherit default :background ,vvsubtle))))
   `(company-tooltip-common            ((,class (:inherit font-lock-constant-face))))
   `(company-tooltip-search            ((,class (:foreground ,search-fg :background ,highlight))))
   `(company-tooltip-selection         ((,class (:background ,vsubtle))))
   `(company-tooltip-mouse             ((,class (:background ,vsubtle))))
   `(company-scrollbar-bg              ((,class (:background ,vsubtle))))
   `(company-scrollbar-fg              ((,class (:background ,subtle))))

   ;; VCS
   `(diff-hl-change                    ((,class (:background ,vc-modified))))
   `(diff-hl-delete                    ((,class (:background ,vc-deleted))))
   `(diff-hl-insert                    ((,class (:background ,vc-added))))
   `(git-gutter+-modified              ((,class (:foreground ,vc-modified :background nil))))
   `(git-gutter+-added                 ((,class (:foreground ,vc-added :background nil))))
   `(git-gutter+-deleted               ((,class (:foreground ,vc-deleted :background nil))))

   ;; Rainbow delimiters
   `(rainbow-delimiters-depth-1-face               ((,class (:foreground ,keywords :bold t))))
   `(rainbow-delimiters-unmatched-face             ((,class (:foreground ,background :background ,orange))))

   ;; Search
   `(isearch                                       ((,class (:foreground ,search-fg :background ,search))))
   `(isearch-lazy-highlight-face                   ((,class (:foreground ,foreground :background ,search-rest))))
   `(evil-snipe-first-match-face                   ((,class (:foreground ,search-fg :background ,highlight))))
   `(evil-snipe-matches-face                       ((,class (:foreground ,highlight :underline t))))
   `(evil-search-highlight-persist-highlight-face  ((,class (:background ,search-rest))))

   `(flyspell-incorrect        ((,class (:underline ,error-highlight :inherit unspecified))))

   `(helm-source-header        ((,class (:background ,background :foreground ,background :height 0.1))))
   `(helm-selection            ((,class (:background ,vvsubtle))))

   `(yascroll:thumb-fringe            ((,class (:background ,vvsubtle :foreground ,vvsubtle))))

   ;; lang-specific
   ;; *****************************************************************************************

   ;; js2-mode
   `(js2-function-param                ((t (:foreground ,variables))))
   `(js2-jsdoc-tag                     ((t (:foreground ,comments :weight bold :bold t))))

   ;; org-mode
   ;; `(org-table ((,class (:inherit 'fixed-pitch))))

   ;; `(org-block ((,class (:height 0.75 :background ,vsubtle))))
   `(org-block-begin-line ((,class (:background ,current-line :foreground ,vsubtle))))
   `(org-block-end-line   ((,class (:inherit org-block-begin-line))))

   `(bold   ((,class (:weight bold :foreground ,white))))
   `(italic ((,class (:slant italic :foreground ,white))))
   `(bold-italic ((,class (:weight bold :slant italic :foreground ,white))))

   `(org-document-title ((,class (:inherit variable-pitch :foreground ,foreground :background ,current-line :height 1.25 :bold nil))))
   `(org-level-1 ((,class (:foreground ,orange :bold t))))
   `(org-level-2 ((,class (:foreground ,methods))))
   `(org-level-3 ((,class (:foreground ,yellow))))
   `(org-level-4 ((,class (:foreground ,cyan))))
   `(org-level-5 ((,class ())))
   `(org-level-6 ((,class ())))
   `(org-level-7 ((,class ())))
   `(org-level-8 ((,class ())))
   `(org-checkbox ((,class (:box (:line-width 1 :style released-button)))))
   `(org-link ((t (:underline t :foreground ,blue))))
   ))


;; *****************************************************************************************

(provide-theme 'narf-dark)

;; Local Variables:
;; no-byte-compile: t
;; End:
