;; NARF Dark
;; By Henrik Lissner <http://github.com/hlissner/emacs.d>

(deftheme narf-dark "A dark theme for narfy emacs, inspired by Molokai")

(custom-theme-set-variables 'narf-dark)

(let* ((c '((class color)))

       (bg             "#1E2021")
       (fg             "#D6D6D4")
       (subtle         "#aab6c7")
       (vsubtle        "#556172")
       (vvsubtle       "#354152")
       (dim-highlight  "#3f4b56")

       (black          "#000000")
       (grey           "#C0C5CF")
       (grey-.5        "#828284")
       (grey-1         "#525254")
       (grey-2         "#36363A")
       (white          "#FFFFFF")
       (white-1        "#EAEADB")
       (yellow         "#E2C770")
       (orange         "#FD971F")
       (red            "#e74c3c")
       (magenta        "#F92672")
       (violet         "#9C91E4")
       (blue           "#268BD2")
       (blue+2         "#727280")
       (cyan           "#66D9EF")
       (green          "#B6E63E")
       (green-3        "#86B20E")
       (dark-cyan      "#8fa1b3")
       (light-cyan     "#CBECFF")

       (search-bg      magenta)
       (search-fg      black)
       (search-rest-bg orange)
       (search-rest-fg black)
       (highlight      orange)
       (vertical-bar   grey-2)
       (current-line   "#262829")
       (selection      "#535556")
       (builtin        orange)
       (comments       grey-1)
       (constants      green)
       (delimiters     "#c0c5ce")
       (functions      cyan)
       (keywords       magenta)
       (methods        dark-cyan)
       (operators      violet)
       (type           cyan)
       (strings        green)
       (variables      orange)

       (error-highlight red)

       (linum-bg       current-line)
       (linum-fg       grey-1)
       (linum-hl-fg    orange)
       (linum-hl-bg    current-line)

       (modeline-fg    white)
       (modeline-fg-2  orange)
       (modeline-fg-3  orange)
       (modeline-fg-inactive  "#80858F")
       (modeline-bg    grey-2)
       (modeline-bg-2  grey-2)
       (modeline-bg-3  grey-2)
       (modeline-bg-inactive  black)

       (vc-modified    grey-2)
       (vc-added       green-3)
       (vc-deleted     red))

  (custom-theme-set-faces
   'narf-dark

   ;; Text
   `(default                         ((,c (:foreground ,fg :background ,bg))))
   `(fringe                          ((,c (:background ,bg :foreground ,grey-1))))
   `(cursor                          ((,c (:background ,white))))
   `(hl-line                         ((,c (:background ,current-line))))
   `(region                          ((,c (:background ,grey-2 :foreground ,white))))
   `(highlight                       ((,c (:foreground ,yellow :inverse-video t))))
   `(shadow                          ((,c (:foreground ,orange))))
   ;; `(secondary-selection          ((,c (:background ,orange))))
   ;; `(lazy-highlight               ((,c (:background ,orange))))
   ;; `(match                        ((,c (:background ,magenta))))
   `(scroll-bar                      ((,c (:background ,green))))

   `(error                           ((,c (:foreground ,red    :bold t))))
   `(warning                         ((,c (:foreground ,yellow :bold t))))
   `(success                         ((,c (:foreground ,green  :bold t))))

   `(spaceline-flycheck-error        ((,c (:bold t :foreground ,red))))
   `(spaceline-flycheck-warning      ((,c (:bold t :foreground ,yellow))))
   `(spaceline-flycheck-info         ((,c (:bold t :foreground ,green))))

   `(hs-face                         ((,c (:foreground ,comments :background ,black))))
   `(hs-fringe-face                  ((,c (:foreground ,orange))))

   `(font-lock-builtin-face          ((,c (:foreground ,builtin))))
   `(font-lock-comment-face          ((,c (:foreground ,comments))))
   `(font-lock-comment-delimiter-face ((,c (:foreground ,comments))))
   `(font-lock-doc-face              ((,c (:foreground ,blue+2))))
   `(font-lock-doc-string-face       ((,c (:foreground ,blue+2))))
   `(font-lock-constant-face         ((,c (:foreground ,constants))))
   `(font-lock-function-name-face    ((,c (:foreground ,functions))))
   `(font-lock-keyword-face          ((,c (:foreground ,keywords))))
   `(font-lock-string-face           ((,c (:foreground ,strings))))
   `(font-lock-type-face             ((,c (:foreground ,type))))
   `(font-lock-variable-name-face    ((,c (:foreground ,variables))))
   `(font-lock-warning-face          ((,c (:foreground ,red))))
   `(font-lock-negation-char-face    ((,c (:foreground ,operators))))
   `(font-lock-preprocessor-char-face      ((,c (:foreground ,operators))))
   `(font-lock-regexp-grouping-backslash   ((,c (:foreground ,operators))))
   `(font-lock-regexp-grouping-construct   ((,c (:foreground ,operators))))

   `(bold                           ((,c (:weight bold  :foreground ,white))))
   `(italic                         ((,c (:slant italic :foreground ,subtle))))
   `(bold-italic                    ((,c (:weight bold  :slant italic :foreground ,white))))

   `(trailing-whitespace            ((,c (:background "#884444"))))
   `(whitespace-tab                 ((,c (:foreground ,grey-2))))
   `(whitespace-newline             ((,c (:foreground ,grey-2))))
   `(whitespace-trailing            ((,c (:background ,grey-2))))

   `(vertical-border                ((,c (:foreground ,vertical-bar :background ,vertical-bar))))

   `(linum                          ((,c (:foreground ,linum-fg :bold nil :height 0.9))))
   `(linum-highlight-face           ((,c (:inherit linum :foreground ,linum-hl-fg))))
   `(show-paren-match               ((,c (:foreground ,highlight :bold t :inverse-video t))))

   ;; Modeline
   `(mode-line                      ((,c (:foreground ,modeline-fg          :background ,modeline-bg))))
   `(mode-line-inactive             ((,c (:foreground ,modeline-fg-inactive :background ,modeline-bg-inactive))))
   `(mode-line-is-modified          ((,c (:foreground ,magenta :bold t))))
   `(mode-line-buffer-file          ((,c (:foreground ,modeline-fg))))
   `(powerline-active1              ((,c (:foreground ,modeline-fg-2 :background ,modeline-bg-2))))
   `(powerline-active2              ((,c (:foreground ,modeline-fg-3 :background ,modeline-bg-3))))
   `(powerline-inactive1            ((,c (:foreground ,modeline-fg-inactive))))
   `(powerline-inactive2            ((,c (:foreground ,modeline-fg-inactive))))
   `(spaceline-highlight-face       ((,c (:foreground ,black :background ,highlight :bold t))))
   `(mode-line-count-face           ((,c (:foreground ,black :background ,magenta :bold t))))

   ;; Search
   `(isearch                        ((,c (:foreground ,search-fg :background ,search-bg))))
   `(isearch-lazy-highlight-face    ((,c (:foreground ,search-rest-fg :background ,search-rest-bg))))

   `(narf-todo-face                 ((,c (:foreground ,yellow :bold t))))
   `(narf-fixme-face                ((,c (:foreground ,red :bold t))))
   `(narf-note-face                 ((,c (:foreground ,cyan :bold t))))

   `(evil-ex-substitute-replacement ((,c (:foreground ,magenta :background ,black :bold t))))
   `(evil-search-highlight-persist-highlight-face ((,c (:background ,search-rest-bg))))


   ;; plugin-specific
   ;; *****************************************************************************************

   `(yascroll:thumb-fringe       ((,c (:background ,grey-1 :foreground ,grey-1))))

   `(reb-match-0                 ((,c (:foreground ,orange :inverse-video t))))
   `(reb-match-1                 ((,c (:foreground ,magenta :inverse-video t))))
   `(reb-match-2                 ((,c (:foreground ,green :inverse-video t))))
   `(reb-match-3                 ((,c (:foreground ,yellow :inverse-video t))))

   ;; neotree
   `(neo-root-dir-face           ((,c (:foreground ,cyan))))
   `(neo-file-link-face          ((,c (:foreground ,white))))
   `(neo-dir-link-face           ((,c (:foreground ,orange))))
   `(neo-expand-btn-face         ((,c (:foreground ,magenta))))

   ;; company-mode
   `(company-tooltip             ((,c (:background ,black :foreground ,fg))))
   `(company-tooltip-common      ((,c (:foreground ,orange))))
   `(company-tooltip-search      ((,c (:foreground ,search-fg :background ,highlight))))
   `(company-tooltip-selection   ((,c (:background ,selection))))
   `(company-tooltip-mouse       ((,c (:background ,magenta :foreground ,bg))))
   `(company-scrollbar-bg        ((,c (:background ,black))))
   `(company-scrollbar-fg        ((,c (:background ,orange))))
   `(company-preview             ((,c (:foreground ,orange))))
   `(company-preview-common      ((,c (:foreground ,magenta :background ,grey-1))))
   `(company-preview-search      ((,c (:inherit company-tooltip-search))))

   `(popup                       ((,c (:inherit company-tooltip))))
   `(popup-tip-face              ((,c (:inherit company-tooltip))))

   ;; evil-snipe
   `(evil-snipe-first-match-face ((,c (:foreground ,search-fg :background ,search-bg))))
   `(evil-snipe-matches-face     ((,c (:foreground ,search-bg :underline t))))

   ;; Volatile highlights
   `(vhl/default-face            ((,c (:background ,grey-2))))

   ;; VCS
   `(diff-hl-change              ((,c (:foreground ,vc-modified))))
   `(diff-hl-delete              ((,c (:foreground ,vc-deleted))))
   `(diff-hl-insert              ((,c (:foreground ,vc-added))))
   ;; `(git-gutter+-modified                 ((,c (:foreground ,vc-modified :background nil))))
   ;; `(git-gutter+-added                    ((,c (:foreground ,vc-added :background nil))))
   ;; `(git-gutter+-deleted                  ((,c (:foreground ,vc-deleted :background nil))))

   ;; Rainbow delimiters
   `(rainbow-delimiters-depth-1-face   ((,c (:foreground ,magenta :bold t))))
   `(rainbow-delimiters-depth-2-face   ((,c (:foreground ,orange))))
   `(rainbow-delimiters-depth-3-face   ((,c (:foreground ,yellow))))
   `(rainbow-delimiters-depth-4-face   ((,c (:foreground ,green))))
   `(rainbow-delimiters-depth-5-face   ((,c (:foreground ,cyan))))
   `(rainbow-delimiters-unmatched-face ((,c (:foreground ,red :inverse-video t))))

   `(flyspell-incorrect ((,c (:underline (:style wave :color ,error-highlight) :inherit unspecified))))

   ;; Helm
   `(helm-source-header          ((,c (:background ,current-line :foreground ,grey-1))))
   `(helm-selection              ((,c (:background ,selection))))
   `(helm-swoop-target-line-face ((,c (:foreground ,highlight :inverse-video t))))
   `(helm-match ((,c (:foreground ,magenta))))

   `(helm-ff-file ((,c (:foreground ,grey))))
   `(helm-ff-prefix ((,c (:foreground ,magenta))))
   `(helm-ff-dotted-directory ((,c (:foreground ,grey-1))))
   `(helm-ff-directory ((,c (:foreground ,orange :bold t))))
   `(helm-ff-executable ((,c (:foreground ,white :slant italic))))


   ;; Avy
   `(avy-lead-face-0    ((,c (:background ,orange :foreground ,black))))
   `(avy-lead-face-1    ((,c (:background ,orange :foreground ,black))))
   `(avy-lead-face-2    ((,c (:background ,orange :foreground ,black))))
   `(avy-lead-face      ((,c (:background ,orange :foreground ,black))))

   ;; lang-specific
   ;; *****************************************************************************************
   ;; js2-mode
   `(js2-function-param ((,c (:foreground ,variables))))
   `(js2-jsdoc-tag      ((,c (:foreground ,comments :bold t))))

   ;; markdown-mode
   `(markdown-header-face           ((,c (:foreground ,orange :bold t))))
   `(markdown-header-delimiter-face ((,c (:foreground ,orange :bold t))))
   `(markdown-blockquote-face ((,c (:foreground ,blue+2))))
   `(markdown-markup-face ((,c (:foreground ,cyan))))
   `(markdown-inline-face ((,c (:foreground ,cyan))))
   `(markdown-list-face ((,c (:foreground ,magenta))))
   `(markdown-pre-face ((,c (:foreground ,cyan))))
   `(markdown-header-face-1 ((,c (:inherit markdown-header-face))))
   `(markdown-header-face-2 ((,c (:inherit markdown-header-face))))
   `(markdown-header-face-3 ((,c (:inherit markdown-header-face))))
   `(markdown-header-face-4 ((,c (:inherit markdown-header-face))))
   `(markdown-header-face-5 ((,c (:inherit markdown-header-face))))
   `(markdown-header-face-6 ((,c (:inherit markdown-header-face))))
   ;; `(markdown-header-rule-face       (:inherit shadow))
   ;; `(markdown-italic-face            (:inherit italic))
   ;; `(markdown-link-face              (:inherit shadow))
   ;; `(markdown-link-title-face        (:inherit link))
   ;; `(markdown-url-face               (:inherit link))

   ;; org-mode
   `(variable-pitch  ((,c (:font "DejaVu Sans" :height 1.0))))
   ;; `(fixed-pitch     ((,c (:font "DejaVu Sans Mono" :height 1.0))))
   `(org-tag         ((,c (:foreground ,grey-1))))
   ;; `(org-ellipsis    ((,c (:inherit hs-face))))

   `(org-table                  ((,c (:foreground ,cyan))))
   `(org-quote                  ((,c (:slant italic :foreground ,grey :background ,current-line))))
   `(org-document-info          ((,c (:foreground ,orange))))
   `(org-document-info-keyword  ((,c (:foreground ,grey-1))))
   `(org-meta-line              ((,c (:background ,current-line :foreground ,vsubtle))))
   `(org-block-begin-line       ((,c (:background ,current-line :foreground ,vsubtle))))
   `(org-block-end-line         ((,c (:inherit org-block-begin-line))))

   `(org-document-title   ((,c (:inherit variable-pitch :foreground ,cyan :height 1.30 :bold t))))
   `(org-level-1          ((,c (:foreground ,orange :bold t))))
   `(org-level-2          ((,c (:foreground ,dark-cyan :bold t))))
   `(org-level-3          ((,c (:foreground ,cyan :bold t))))
   `(org-level-4          ((,c (:foreground ,green :bold t))))
   `(org-level-5          ((,c (:foreground ,cyan))))
   `(org-level-6          ((,c (:foreground ,blue+2))))
   ;; `(org-level-7          ((,c ())))
   ;; `(org-level-8          ((,c ())))
   ;;`(org-checkbox         ((,class (:box (:line-width 1 :style released-button)))))

   `(org-code             ((,c (:foreground ,orange))))
   `(org-verbatim         ((,c (:foreground ,green))))
   `(org-formula          ((,c (:foreground ,cyan))))
   `(org-list-dt          ((,c (:foreground ,cyan :bold t))))
   `(org-footnote         ((,c (:foreground ,orange))))

   `(org-link             ((,c (:underline t :foreground ,yellow :bold inherit))))
   `(org-date             ((,c (:foreground ,violet))))
   `(org-todo             ((,c (:foreground ,yellow :bold inherit))))
   `(org-done             ((,c (:foreground ,green :bold inherit))))
   `(org-headline-done    ((,c (:foreground ,grey-.5 :strike-through t :bold nil))))
   `(org-special-keyword  ((,c (:foreground ,magenta))))
   `(org-checkbox-statistics-todo ((,c (:inherit org-todo))))
   `(org-checkbox-statistics-done ((,c (:inherit org-done))))

   ;; NARF custom org faces
   `(org-headline-todo    ((,c (:foreground ,dark-cyan :bold nil))))
   `(org-block            ((,c (:background ,current-line))))
   `(org-block-background ((,c (:background ,current-line))))
   `(org-todo-high        ((,c (:foreground ,orange :bold inherit))))
   `(org-todo-vhigh       ((,c (:foreground ,magenta :bold inherit))))
   `(org-list-bullet      ((,c (:foreground ,orange :bold t))))
   `(org-whitespace       ((,c (:inherit fixed-pitch))))
   `(org-todo-checkbox    ((,c (:inherit variable-pitch))))

   ))


;; *****************************************************************************************

(provide-theme 'narf-dark)

;; Local Variables:
;; no-byte-compile: t
;; End:
