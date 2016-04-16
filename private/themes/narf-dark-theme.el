;; NARF Dark
;; By Henrik Lissner <http://github.com/hlissner/emacs.d>

(require 'dash)

(deftheme narf-dark "A dark theme for narfy emacs, inspired by Molokai")

;; Color helper functions
;; Shamelessly *borrowed* from solarized
(defun --color-name-to-rgb (color &optional frame)
  (let ((valmax (float (car (color-values "#ffffff")))))
    (mapcar (lambda (x) (/ x valmax)) (color-values color frame))))

(defun --color-rgb-to-hex  (red green blue)
  (format "#%02x%02x%02x"
          (* red 255) (* green 255) (* blue 255)))

(defun --color-blend (color1 color2 alpha)
  (apply '--color-rgb-to-hex
         (-zip-with '(lambda (it other)
                       (+ (* alpha it) (* other (- 1 alpha))))
                    (--color-name-to-rgb color1)
                    (--color-name-to-rgb color2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let* ((c '((class color)))

       ;; Global bold flag
       (bold           t)

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
       (grey-2         "#39393D")
       (white          "#FFFFFF")
       (white-1        "#EAEADB")
       (yellow         "#E2C770")
       (orange         "#FD971F")
       (red            "#E74C3C")
       (magenta        "#F92672")
       (violet         "#9C91E4")
       (blue           "#268BD2")
       (blue+2         "#727280")
       (cyan           "#66D9EF")
       (green          "#B6E63E")
       (green-3        "#86B20E")
       (dark-cyan      "#8FA1B3")
       (light-cyan     "#CBECFF")

       (search-bg      green)
       (search-fg      black)
       (search-rest-bg violet)
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
       (linum-fg       "#3F3F48")
       (linum-hl-fg    orange)
       (linum-hl-bg    current-line)

       (active-minibuffer "#404046")
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
   `(minibuffer-prompt               ((,c (:foreground ,orange))))

   `(error                           ((,c (:foreground ,red   ))))
   `(warning                         ((,c (:foreground ,yellow))))
   `(success                         ((,c (:foreground ,green ))))

   `(spaceline-flycheck-error        ((,c (:underline nil :foreground ,black :background ,red))))
   `(spaceline-flycheck-warning      ((,c (:underline nil :foreground ,black :background ,yellow))))
   `(spaceline-flycheck-info         ((,c (:underline nil :foreground ,black :background ,green))))
   `(flycheck-error                  ((,c (:underline (:style wave :color ,red)    :background ,grey-2))))
   `(flycheck-warning                ((,c (:underline (:style wave :color ,yellow) :background ,grey-2))))
   `(flycheck-info                   ((,c (:underline (:style wave :color ,green)  :background ,grey-2))))
   `(flyspell-incorrect              ((,c (:underline (:style wave :color ,error-highlight)
                                           :inherit unspecified))))

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

   ;; `(linum                          ((,c (:foreground ,linum-fg :bold nil :height 0.8))))
   `(linum                          ((,c (:foreground ,linum-fg :bold nil))))
   `(linum-highlight-face           ((,c (:inherit linum :foreground ,linum-hl-fg))))
   `(show-paren-match               ((,c (:foreground ,magenta :inverse-video t))))

   ;; Modeline
   `(narf-minibuffer-active         ((,c (:background ,active-minibuffer))))
   `(mode-line                      ((,c (:foreground ,modeline-fg          :background ,modeline-bg))))
   `(mode-line-inactive             ((,c (:foreground ,modeline-fg-inactive :background ,modeline-bg-inactive))))

   `(mode-line-is-modified          ((,c (:foreground ,magenta))))
   `(mode-line-buffer-file          ((,c (:foreground ,modeline-fg :background nil))))
   `(powerline-active1              ((,c (:foreground ,modeline-fg-2 :background ,modeline-bg-2))))
   `(powerline-active2              ((,c (:foreground ,modeline-fg-3 :background ,modeline-bg-3))))
   `(powerline-inactive1            ((,c (:foreground ,modeline-fg-inactive :background ,modeline-bg-inactive))))
   `(powerline-inactive2            ((,c (:foreground ,modeline-fg-inactive :background ,modeline-bg-inactive))))
   `(spaceline-highlight-face       ((,c (:foreground ,black :background ,highlight))))
   `(mode-line-count-face           ((,c (:foreground ,black :background ,magenta))))

   ;; Search
   `(isearch                        ((,c (:foreground ,search-fg :background ,search-bg))))
   `(isearch-lazy-highlight-face    ((,c (:foreground ,search-rest-fg :background ,search-rest-bg))))

   `(narf-todo-face                 ((,c (:foreground ,yellow))))
   `(narf-fixme-face                ((,c (:foreground ,red))))
   `(narf-note-face                 ((,c (:foreground ,cyan))))

   `(evil-ex-substitute-replacement ((,c (:foreground ,magenta :background ,black :bold ,bold))))
   `(evil-search-highlight-persist-highlight-face ((,c (:background ,search-rest-bg))))

   ;; plugin-specific
   ;; *****************************************************************************************

   `(highlight-indentation-face                 ((,c (:background ,current-line))))
   `(highlight-indentation-current-column-face  ((,c (:background ,current-line))))
   `(indent-guide-face                          ((,c (:foreground "#2F2F38"))))

   `(highlight-quoted-symbol     ((,c (:foreground ,yellow))))
   `(highlight-quoted-quote      ((,c (:foreground ,magenta))))
   `(highlight-numbers-number    ((,c (:foreground ,constants))))

   `(reb-match-0                 ((,c (:foreground ,orange   :inverse-video t))))
   `(reb-match-1                 ((,c (:foreground ,magenta  :inverse-video t))))
   `(reb-match-2                 ((,c (:foreground ,green    :inverse-video t))))
   `(reb-match-3                 ((,c (:foreground ,yellow   :inverse-video t))))

   ;; workgroups2
   `(wg-current-workgroup-face   ((,c (:foreground ,black   :background ,orange))))
   `(wg-other-workgroup-face     ((,c (:foreground ,grey-.5 :background ,current-line))))

   ;; neotree
   `(neo-root-dir-face           ((,c (:foreground ,cyan))))
   `(neo-file-link-face          ((,c (:foreground ,white))))
   `(neo-dir-link-face           ((,c (:foreground ,orange))))
   `(neo-expand-btn-face         ((,c (:foreground ,magenta))))

   ;; company-mode
   `(tooltip                     ((,c (:background ,grey-2 :foreground ,orange))))

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

   `(popup                       ((,c (:inherit tooltip))))
   `(popup-tip-face              ((,c (:inherit tooltip))))

   ;; evil-snipe
   `(evil-snipe-first-match-face ((,c (:foreground ,search-fg :background ,search-bg))))
   `(evil-snipe-matches-face     ((,c (:foreground ,search-bg :underline t))))

   ;; Volatile highlights
   `(vhl/default-face            ((,c (:background ,grey-2))))

   ;; VCS
   `(diff-hl-change              ((,c (:foreground ,vc-modified))))
   `(diff-hl-delete              ((,c (:foreground ,vc-deleted))))
   `(diff-hl-insert              ((,c (:foreground ,vc-added))))
   `(git-gutter:modified         ((,c (:foreground ,vc-modified))))
   `(git-gutter:added            ((,c (:foreground ,vc-added))))
   `(git-gutter:deleted          ((,c (:foreground ,vc-deleted))))
   `(git-gutter+-modified        ((,c (:foreground ,vc-modified :background nil))))
   `(git-gutter+-added           ((,c (:foreground ,vc-added :background nil))))
   `(git-gutter+-deleted         ((,c (:foreground ,vc-deleted :background nil))))

   ;; Rainbow delimiters
   `(rainbow-delimiters-depth-1-face   ((,c (:foreground ,magenta))))
   `(rainbow-delimiters-depth-2-face   ((,c (:foreground ,orange))))
   `(rainbow-delimiters-depth-3-face   ((,c (:foreground ,yellow))))
   `(rainbow-delimiters-depth-4-face   ((,c (:foreground ,green))))
   `(rainbow-delimiters-depth-5-face   ((,c (:foreground ,cyan))))
   `(rainbow-delimiters-unmatched-face ((,c (:foreground ,red :inverse-video t))))

   ;; Helm
   `(helm-selection              ((,c (:background ,selection))))
   `(helm-match                  ((,c (:foreground ,magenta))))
   `(helm-source-header          ((,c (:background ,current-line :foreground ,grey-1))))
   `(helm-swoop-target-line-face ((,c (:foreground ,highlight :inverse-video t))))

   `(helm-ff-file              ((,c (:foreground ,grey))))
   `(helm-ff-prefix            ((,c (:foreground ,magenta))))
   `(helm-ff-dotted-directory  ((,c (:foreground ,grey-1))))
   `(helm-ff-directory         ((,c (:foreground ,orange))))
   `(helm-ff-executable        ((,c (:foreground ,white :slant italic))))

   ;; Avy
   `(avy-lead-face-0    ((,c (:background ,orange :foreground ,black))))
   `(avy-lead-face-1    ((,c (:background ,orange :foreground ,black))))
   `(avy-lead-face-2    ((,c (:background ,orange :foreground ,black))))
   `(avy-lead-face      ((,c (:background ,orange :foreground ,black))))

   ;; lang-specific
   ;; *****************************************************************************************
   ;; (css|scss)-mode
   `(css-proprietary-property ((,c (:foreground ,keywords))))

   ;; js2-mode
   `(js2-function-param  ((,c (:foreground ,variables))))
   `(js2-function-call   ((,c (:foreground ,functions))))
   `(js2-object-property ((,c (:foreground ,methods))))
   `(js2-jsdoc-tag       ((,c (:foreground ,comments))))

   ;; web-mode
   `(web-mode-doctype-face           ((,c (:foreground ,comments))))
   `(web-mode-html-tag-face          ((,c (:foreground ,methods))))
   `(web-mode-html-tag-bracket-face  ((,c (:foreground ,methods))))
   `(web-mode-html-attr-name-face    ((,c (:foreground ,type))))
   `(web-mode-block-control-face     ((,c (:foreground ,orange))))
   ;; `(web-mode-html-tag-bracket-face  ((,c (:foreground ,operators))))

   ;; markdown-mode
   `(markdown-header-face           ((,c (:foreground ,orange))))
   `(markdown-header-delimiter-face ((,c (:foreground ,orange))))
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
   `(variable-pitch  ((,c (:font "Proxima Nova"))))
   `(fixed-pitch     ((,c (:font "DejaVu Sans Mono" :height 1.0))))
   `(org-tag         ((,c (:foreground ,yellow :bold nil))))
   ;; `(org-ellipsis    ((,c (:inherit hs-face))))

   `(org-hide        ((,c (:foreground ,bg))))

   `(org-table                  ((,c (:foreground ,cyan))))
   `(org-quote                  ((,c (:slant italic :foreground ,grey :background ,current-line))))
   `(org-document-info          ((,c (:foreground ,orange))))
   `(org-document-info-keyword  ((,c (:foreground ,grey-1))))
   `(org-meta-line              ((,c (:foreground ,vsubtle))))
   `(org-block-begin-line       ((,c (:background ,current-line :foreground ,vsubtle))))
   `(org-block-end-line         ((,c (:inherit org-block-begin-line))))
   `(org-block-background       ((,c (:background ,current-line))))
   `(org-archived               ((,c (:foreground ,grey-.5))))

   `(org-document-title   ((,c (:foreground ,cyan))))
   `(org-level-1          ((,c (:background ,current-line :foreground ,magenta :bold ,bold))))
   `(org-level-2          ((,c (                          :foreground ,orange :bold ,bold))))
   `(org-level-3          ((,c (                          :foreground ,orange :bold ,bold))))
   `(org-level-4          ((,c (                          :foreground ,orange :bold ,bold))))
   `(org-level-5          ((,c (                          :foreground ,orange))))
   `(org-level-6          ((,c (                          :foreground ,orange))))
   ;;`(org-level-7          ((,c ())))
   ;;`(org-level-8          ((,c ())))
   ;;`(org-checkbox         ((,class (:box (:line-width 1 :style released-button)))))

   `(org-code             ((,c (:foreground ,orange))))
   `(org-verbatim         ((,c (:foreground ,green))))
   `(org-formula          ((,c (:foreground ,cyan))))
   `(org-list-dt          ((,c (:foreground ,cyan))))
   `(org-footnote         ((,c (:foreground ,orange))))

   `(org-link             ((,c (:underline t :foreground ,cyan :bold inherit))))
   `(org-date             ((,c (:foreground ,violet))))
   `(org-todo             ((,c (:foreground ,yellow :bold inherit))))
   `(org-done             ((,c (:foreground ,green :bold inherit))))
   `(org-headline-done    ((,c (:foreground ,grey-.5 :strike-through t :bold nil))))
   `(org-special-keyword  ((,c (:foreground ,magenta))))
   `(org-checkbox-statistics-todo ((,c (:inherit org-todo))))
   `(org-checkbox-statistics-done ((,c (:inherit org-done))))

   ;; NARF custom org faces
   `(org-headline-todo    ((,c (:bold nil))))
   `(org-block            ((,c (:background ,current-line))))
   `(org-block-background ((,c (:background ,current-line))))
   `(org-todo-high        ((,c (:foreground ,orange :bold inherit))))
   `(org-todo-vhigh       ((,c (:foreground ,magenta :bold inherit))))
   `(org-list-bullet      ((,c (:foreground ,orange :bold ,bold))))
   `(org-whitespace       ((,c (:inherit fixed-pitch))))
   `(org-todo-checkbox    ((,c (:inherit variable-pitch))))
   )

;; *****************************************************************************************

  (custom-theme-set-variables
   'narf-dark
   `(vc-annotate-color-map
     '((20 .  ,green)
       (40 .  ,(--color-blend yellow green (/ 1.0 3)))
       (60 .  ,(--color-blend yellow green (/ 2.0 3)))
       (80 .  ,yellow)
       (100 . ,(--color-blend orange yellow (/ 1.0 3)))
       (120 . ,(--color-blend orange yellow (/ 2.0 3)))
       (140 . ,orange)
       (160 . ,(--color-blend magenta orange (/ 1.0 3)))
       (180 . ,(--color-blend magenta orange (/ 2.0 3)))
       (200 . ,magenta)
       (220 . ,(--color-blend red magenta (/ 1.0 3)))
       (240 . ,(--color-blend red magenta (/ 2.0 3)))
       (260 . ,red)
       (280 . ,(--color-blend grey-2 red (/ 1.0 4)))
       (300 . ,(--color-blend grey-2 red (/ 2.0 4)))
       (320 . ,(--color-blend grey-2 red (/ 3.0 4)))
       (340 . ,grey-2)
       (360 . ,grey-2)))
   `(vc-annotate-very-old-color nil)
   `(vc-annotate-background ,black))
  )

;; *****************************************************************************************

(provide-theme 'narf-dark)

;; Local Variables:
;; no-byte-compile: t
;; End:
