;; NARF Dark
;; By Henrik Lissner <http://github.com/hlissner/emacs.d>

(deftheme narf-dark "dark theme for narfy emacs")

(custom-theme-set-variables 'narf-dark)

(let* ((class '((class color) (min-colors 89)))
       (white            "#ffffff")
       (off-white        "#eaeadb")

       (background       "#242836")
       (foreground       "#cccada")
       (subtle           "#aab6c7")
       (vsubtle          "#556172")
       (vvsubtle         "#354152")
       (highlight        "orange")
       (error-highlight  "#55ffff")

       (vertical-bar     "#000000")
       (current-line     "#2c354c")
       (selection        "#445f7f")
       (search-fg        "#000000")
       (search           "#cccccc")
       (search-rest      "#545060")
       (linum-bg         "#55616A")
       (linum-fg         background)
       (linum-highlight  foreground)

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

       (modeline-bg-main "#171c26")
       (modeline-bg-light "#363c4d")
       (modeline-bg-lighter "#3f475d")
       (modeline-bg-inactive  "#2b303f")

       (vc-modified      "#55616A")
       (vc-added         "#436b3b")
       (vc-deleted       "#714243")

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
   `(font-lock-constant-face           ((,class (:foreground ,constants))))
   `(font-lock-doc-face                ((,class (:foreground ,comments))))
   `(font-lock-doc-string-face         ((,class (:foreground ,comments))))
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
   `(vertical-border           ((,class (:foreground ,vertical-bar) )))

   `(linum                     ((,class (:background ,linum-fg :foreground ,linum-bg) )))
   `(linum-highlight-face      ((,class (:foreground ,linum-highlight :background ,current-line))))
   `(show-paren-match          ((,class (:foreground ,highlight :weight ultra-bold))))

   `(mode-line                 ((,class (:foreground ,subtle :background ,modeline-bg-main))))
   `(powerline-active1         ((,class (:foreground ,subtle :background ,modeline-bg-light))))
   `(powerline-active2         ((,class (:foreground ,subtle :background ,modeline-bg-lighter))))
   `(mode-line-is-modified     ((,class (:foreground ,highlight))))
   `(mode-line-buffer-file     ((,class (:foreground ,off-white))))
   `(mode-line-buffer-dir      ((,class (:foreground ,subtle))))

   `(mode-line-inactive        ((,class (:foreground ,vsubtle :background ,modeline-bg-inactive))))
   `(powerline-inactive1       ((,class (:foreground ,vsubtle :background ,modeline-bg-inactive))))
   `(powerline-inactive2       ((,class (:foreground ,vsubtle :background ,modeline-bg-inactive))))

   ;; company-mode
   `(company-tooltip                   ((,class (:inherit default :background "#3e4555"))))
   `(company-tooltip-selection         ((,class (:inherit font-lock-function-name-face))))
   `(company-tooltip-common            ((,class (:inherit font-lock-constant-face))))
   `(company-scrollbar-bg              ((,class (:background "#4b5367"))))
   `(company-scrollbar-fg              ((,class (:background "#353b49"))))
   `(company-search                    ((,class (:background "#4b5367"))))

   ;; VCS
   `(diff-hl-change                                ((,class (:background ,vc-modified))))
   `(diff-hl-delete                                ((,class (:background ,vc-deleted))))
   `(diff-hl-insert                                ((,class (:background ,vc-added))))
   `(git-gutter+-modified                          ((,class (:foreground ,vc-modified :background nil))))
   `(git-gutter+-added                             ((,class (:foreground ,vc-added :background nil))))
   `(git-gutter+-deleted                           ((,class (:foreground ,vc-deleted :background nil))))

   ;; Rainbow delimiters
   `(rainbow-delimiters-depth-1-face               ((,class (:foreground "#CCCCDD" :weight bold :bold t))))
   `(rainbow-delimiters-unmatched-face             ((,class (:foreground "#FFFFFF" :background "#EF6C00"))))

   `(isearch                                       ((,class (:foreground ,search-fg :background ,search))))
   `(isearch-lazy-highlight-face                   ((,class (:foreground ,foreground :background ,search-rest))))
   `(evil-snipe-first-match-face                   ((,class (:foreground ,search-fg :background ,highlight))))
   `(evil-snipe-matches-face                       ((,class (:foreground ,highlight :underline t))))
   `(evil-search-highlight-persist-highlight-face  ((,class (:background ,search-rest))))

   `(flyspell-incorrect        ((,class (:underline ,error-highlight :inherit unspecified))))

   `(helm-source-header        ((,class (:background ,modeline-bg-light :foreground ,strings :weight bold :height 1.0))))
   `(helm-selection            ((,class (:background ,modeline-bg-lighter))))

   `(yascroll:thumb-fringe            ((,class (:background ,vvsubtle :foreground ,vvsubtle))))

   ;; lang-specific
   ;; *****************************************************************************************

   ;; js2-mode
   `(js2-function-param                ((t (:foreground ,variables))))
   `(js2-jsdoc-tag                     ((t (:foreground ,comments :weight bold :bold t))))
   ;; org-mode
   `(org-level-1                      ((t (:inherit outline-1 :bold t :foreground ,constants))))
   `(org-level-2                      ((t (:inherit outline-2 :bold t :foreground ,variables))))
   ))


;; *****************************************************************************************

(provide-theme 'narf-dark)

;; Local Variables:
;; no-byte-compile: t
;; End:
