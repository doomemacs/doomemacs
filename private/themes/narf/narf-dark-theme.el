;; NARF Dark
;; By Henrik Lissner <http://github.com/hlissner/emacs.d>

(deftheme narf-dark "A dark theme for narfy emacs, inspired by Molokai.")

(custom-theme-set-variables 'narf-dark)

(let* ((class '((class color)))

       (background            "#1E2021")
       (foreground            "#D6D6D4")
       (subtle                "#aab6c7")
       (vsubtle               "#556172")
       (vvsubtle              "#354152")
       (dim-highlight         "#3f4b56")

       (black                 "#000000")
       (dark-grey             "#525254")
       (faded-grey            "#36363A")
       (grey                  "#C0C5CF")
       (white                 "#FFFFFF")
       (off-white             "#EAEADB")
       (yellow                "#E2D770")
       (orange                "#FD971F")
       (red                   "#DC322F")
       (magenta               "#F92672")
       (violet                "#6C71C4")
       (blue                  "#268BD2")
       (steel-blue            "#727280")
       (cyan                  "#66D9EF")
       (green                 "#B6E23E")
       (dark-cyan             "#8fa1b3")
       (light-cyan            "#CBECFF")

       (search-bg             magenta)
       (search-fg             black)
       (search-rest-bg        orange)
       (search-rest-fg        black)
       (highlight             orange)
       (vertical-bar          black)
       (current-line          "#232526")
       (selection             "#535556")
       (builtin               orange)
       (comments              dark-grey)
       (constants             green)
       (delimiters            "#c0c5ce")
       (functions             cyan)
       (keywords              magenta)
       (methods               dark-cyan)
       (operators             grey)
       (type                  cyan)
       (strings               green)
       (variables             orange)

       (error-highlight       red)

       (linum-bg              current-line)
       (linum-fg              dark-grey)
       (linum-hl-fg           orange)
       (linum-hl-bg           current-line)

       (modeline-fg           white)
       (modeline-fg-2         orange)
       (modeline-fg-3         orange)
       (modeline-fg-inactive  "#80858F")
       (modeline-bg-light     faded-grey)
       (modeline-bg-inactive  black)

       (vc-modified           "#55616A")
       (vc-added              "#437f4b")
       (vc-deleted            "#864253")
       )

  (custom-theme-set-faces
   'narf-dark

   ;; Text
   `(default                              ((,class (:foreground ,foreground :background ,background))))
   `(fringe                               ((,class (:background ,background :foreground ,magenta))))
   `(cursor                               ((,class (:background ,white))))
   `(hl-line                              ((,class (:background ,current-line))))
   `(region                               ((,class (:foreground ,grey :inverse-video t))))
   `(highlight                            ((,class (:foreground ,yellow :inverse-video t))))

   `(font-lock-builtin-face               ((,class (:foreground ,builtin))))
   `(font-lock-comment-delimiter-face     ((,class (:foreground ,comments))))
   `(font-lock-comment-face               ((,class (:foreground ,comments))))
   `(font-lock-doc-face                   ((,class (:foreground ,steel-blue))))
   `(font-lock-doc-string-face            ((,class (:foreground ,steel-blue))))
   `(font-lock-constant-face              ((,class (:foreground ,constants))))
   `(font-lock-function-name-face         ((,class (:foreground ,functions))))
   `(font-lock-keyword-face               ((,class (:foreground ,keywords))))
   ;;`(font-lock-negation-char-face          ((,class ())))
   ;;`(font-lock-preprocessor-char-face      ((,class ())))
   ;;`(font-lock-regexp-grouping-backslash   ((,class ())))
   ;;`(font-lock-regexp-grouping-construct   ((,class ())))
   `(font-lock-string-face                ((,class (:foreground ,strings))))
   `(font-lock-type-face                  ((,class (:foreground ,type))))
   `(font-lock-variable-name-face         ((,class (:foreground ,variables))))
   `(font-lock-warning-face               ((,class (:foreground ,red))))

   `(bold                                 ((,class (:weight bold  :foreground ,white))))
   `(italic                               ((,class (:slant italic :foreground ,subtle))))
   `(bold-italic                          ((,class (:weight bold  :slant italic :foreground ,white))))

   `(trailing-whitespace                  ((,class (:background "#884444"))))
   `(whitespace-tab                       ((,class (:foreground "#343d46"))))
   `(whitespace-newline                   ((,class (:foreground "#343d46"))))
   `(whitespace-trailing                  ((,class (:background "#553333"))))

   `(vertical-border                      ((,class (:foreground ,vertical-bar :background ,vertical-bar))))

   `(linum                                ((,class (:foreground ,linum-fg :bold nil))))
   `(linum-highlight-face                 ((,class (:inherit linum        :foreground ,linum-hl-fg))))
   `(show-paren-match                     ((,class (:foreground ,highlight :weight ultra-bold :inverse-video t))))

   ;; Modeline
   `(mode-line                            ((,class (:foreground ,modeline-fg          :background ,modeline-bg-light))))
   `(mode-line-inactive                   ((,class (:foreground ,modeline-fg-inactive :background ,modeline-bg-inactive))))
   `(mode-line-is-modified                ((,class (:foreground ,highlight))))
   `(mode-line-buffer-file                ((,class (:foreground ,modeline-fg))))
   `(powerline-active1                    ((,class (:foreground ,modeline-fg-2))))
   `(powerline-active2                    ((,class (:foreground ,modeline-fg-3))))
   `(powerline-inactive1                  ((,class (:foreground ,modeline-fg-inactive))))
   `(powerline-inactive2                  ((,class (:foreground ,modeline-fg-inactive))))
   `(spaceline-highlight-face             ((,class (:foreground ,black :background ,highlight))))

   ;; Search
   `(isearch                              ((,class (:foreground ,search-fg :background ,search-bg))))
   `(isearch-lazy-highlight-face          ((,class (:foreground ,search-rest-fg :background ,search-rest-bg))))

   `(narf-todo-face                       ((,class (:foreground ,yellow :bold t))))
   `(narf-fixme-face                      ((,class (:foreground ,red :bold t))))
   `(narf-note-face                       ((,class (:foreground ,cyan :bold t))))
   `(evil-search-highlight-persist-highlight-face
     ((,class (:background ,search-rest-bg))))


   ;; plugin-specific
   ;; *****************************************************************************************

   `(yascroll:thumb-fringe                ((,class (:background ,orange))))

   ;; company-mode
   `(company-tooltip                      ((,class (:background ,black :foreground ,foreground))))
   `(company-tooltip-common               ((,class (:foreground ,orange))))
   `(company-tooltip-search               ((,class (:foreground ,search-fg :background ,highlight))))
   `(company-tooltip-selection            ((,class (:background ,selection))))
   `(company-tooltip-mouse                ((,class (:background ,magenta :foreground ,background))))
   `(company-scrollbar-bg                 ((,class (:background ,black))))
   `(company-scrollbar-fg                 ((,class (:background ,orange))))
   `(company-preview                      ((,class (:foreground ,orange))))
   `(company-preview-common               ((,class (:foreground ,magenta :background ,dark-grey))))
   `(company-preview-search               ((,class (:inherit company-tooltip-search))))

   ;; evil-snipe
   `(evil-snipe-first-match-face          ((,class (:foreground ,search-fg :background ,search-bg))))
   `(evil-snipe-matches-face              ((,class (:foreground ,search-bg :underline t))))

   ;; Volatile highlights
   `(vhl/default-face                     ((,class (:background ,selection))))

   ;; VCS
   `(diff-hl-change                       ((,class (:background ,vc-modified))))
   `(diff-hl-delete                       ((,class (:background ,vc-deleted))))
   `(diff-hl-insert                       ((,class (:background ,vc-added))))
   ;; `(git-gutter+-modified                 ((,class (:foreground ,vc-modified :background nil))))
   ;; `(git-gutter+-added                    ((,class (:foreground ,vc-added :background nil))))
   ;; `(git-gutter+-deleted                  ((,class (:foreground ,vc-deleted :background nil))))

   ;; Rainbow delimiters
   `(rainbow-delimiters-depth-1-face      ((,class (:foreground ,magenta :bold t))))
   `(rainbow-delimiters-depth-2-face      ((,class (:foreground ,orange))))
   `(rainbow-delimiters-depth-3-face      ((,class (:foreground ,yellow))))
   `(rainbow-delimiters-depth-4-face      ((,class (:foreground ,green))))
   `(rainbow-delimiters-depth-5-face      ((,class (:foreground ,cyan))))
   `(rainbow-delimiters-unmatched-face    ((,class (:foreground ,background :background ,red))))

   `(flyspell-incorrect                   ((,class (:underline (:style wave :color ,error-highlight) :inherit unspecified))))

   ;; Helm
   `(helm-source-header                   ((,class (:background ,background :foreground ,background :height 0.1))))
   `(helm-selection                       ((,class (:background ,selection))))

   ;; Avy
   `(avy-lead-face-0                      ((,class (:background ,orange :foreground ,black))))
   `(avy-lead-face-1                      ((,class (:background ,orange :foreground ,black))))
   `(avy-lead-face-2                      ((,class (:background ,orange :foreground ,black))))
   `(avy-lead-face                        ((,class (:background ,orange :foreground ,black))))

   ;; lang-specific
   ;; *****************************************************************************************
   ;; js2-mode
   `(js2-function-param                ((t (:foreground ,variables))))
   `(js2-jsdoc-tag                     ((t (:foreground ,comments :weight bold :bold t))))

   ;; markdown-mode
   `(markdown-header-face   ((,class (:foreground ,orange :bold t))))
   `(markdown-header-face-1 ((,class (:foreground ,dark-cyan))))
   `(markdown-header-face-2 ((,class (:foreground ,yellow))))
   `(markdown-header-face-3 ((,class (:foreground ,cyan))))
   `(markdown-header-face-4 ((,class ())))
   `(markdown-header-face-5 ((,class ())))
   `(markdown-header-face-6 ((,class ())))
   ;; `(markdown-header-rule-face       (:inherit shadow))
   ;; `(markdown-italic-face            (:inherit italic))
   ;; `(markdown-link-face              (:inherit shadow))
   ;; `(markdown-link-title-face        (:inherit link))
   ;; `(markdown-url-face               (:inherit link))

   ;; org-mode
   ;; `(org-table ((,class (:inherit 'fixed-pitch))))
   ;; `(org-block ((,class (:height 0.75 :background ,vsubtle))))
   `(org-block-begin-line ((,class (:background ,current-line :foreground ,vsubtle))))
   `(org-block-end-line   ((,class (:inherit org-block-begin-line))))
   `(org-document-title   ((,class (:inherit variable-pitch :foreground ,foreground :height 1.40 :bold t))))
   `(org-level-1          ((,class (:foreground ,magenta :bold t))))
   `(org-level-2          ((,class (:foreground ,dark-cyan))))
   `(org-level-3          ((,class (:foreground ,yellow))))
   `(org-level-4          ((,class (:foreground ,cyan))))
   `(org-level-5          ((,class (:foreground ,orange))))
   `(org-level-6          ((,class (:foreground ,steel-blue))))
   `(org-level-7          ((,class ())))
   `(org-level-8          ((,class ())))
   ;;`(org-checkbox         ((,class (:box (:line-width 1 :style released-button)))))

   `(org-code             ((t (:foreground ,orange))))
   `(org-verbatim         ((t (:foreground ,green))))

   `(org-link             ((t (:underline t :foreground ,yellow))))
   `(org-todo             ((t (:foreground ,orange))))
   `(org-done             ((t (:foreground ,green))))
   `(org-special-keyword  ((t (:foreground ,magenta))))
   `(org-checkbox-statistics-todo             ((t (:inherit org-todo))))
   `(org-checkbox-statistics-done             ((t (:inherit org-done))))
   ))


;; *****************************************************************************************

(provide-theme 'narf-dark)

;; Local Variables:
;; no-byte-compile: t
;; End:
