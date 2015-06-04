 ;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; *****************************************************************************************
;;
;; v0 :- A dark Emacs theme inspired by Space Grey ST2 theme
;;
;; *****************************************************************************************

;;
;; By Henrik Lissner <http://github.com/hlissner/emacs.d>
;;

(deftheme v0 "V-NOUGHT dark theme for Emacs 24+")

  (custom-theme-set-variables 'v0)

  (let (;; (background       "#222222")
        (background       "#2b303b")
        ;; (gutters       "#262E34")
        (gutters          "#1f252a")
        (gutter-fg        "#55616A")
        ;; (gutters-active   "#2e363c")
        (gutters-active   "#1c1f26")
        (linum            "#1e262c")
        ;; (gutter-light     "#434f58")
        (gutter-light     "#232830")
        (builtin          "#d08770")
        (foreground       "#c0c5ce")
        (invisibles       "#65737e")
        ;; (line-hl          "#353539")
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
   'v0

;; Default colors
;; *****************************************************************************************

   `(default              ((t (:foreground ,text :background ,background) )))
   `(hl-line              ((t (:background ,line-hl) )))
   `(region               ((t (:background ,selection) )))
   `(cursor               ((t (:background ,white) )))
   `(fringe               ((t (:background ,background :foreground ,white) )))
   `(linum                ((t (:background ,background :foreground ,gutter-fg :weight normal) )))

   `(vertical-border      ((t (:foreground "#000000") )))

   `(mode-line            ((t (:foreground ,white
                               :background ,gutter-light
                               :box (:line-width 3 :color ,gutter-light)
                               ))))

   `(mode-line-inactive   ((t (:foreground ,gutter-fg
                               :background ,gutters-active
                               :box (:line-width 3 :color ,gutters-active)
                               ))))

   `(mode-line-modified-face           ((t (:foreground ,builtin))))

   `(sml/folder           ((t nil)))
   `(sml/modified         ((t (:foreground ,highlight))))

   `(flyspell-incorrect   ((t (:underline "#ff5555" :inherit unspecified))))

   `(helm-source-header   ((t (:background ,gutters-active :foreground ,strings :weight bold :height 1.0))))
   `(helm-selection       ((t (:background ,selection))))

;; Font lock faces
;; *****************************************************************************************

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
   `(whitespace-tab                    ((t (:foreground "#444444"))))
   `(whitespace-newline                ((t (:foreground "#444444"))))
   `(whitespace-trailing               ((t (:background "#553333"))))

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


;; *****************************************************************************************

   `(persp-selected-face               ((t (:foreground ,builtin))))

   `(org-level-1                      ((t (:inherit outline-1 :bold t :foreground ,constants))))
   `(org-level-2                      ((t (:inherit outline-2 :bold t :foreground ,variables))))

   `(show-paren-match                 ((t (:background nil :foreground ,highlight :weight ultra-bold))))

   `(evil-snipe-first-match-face      ((t (:background ,highlight :foreground "black"))))
   `(evil-snipe-matches-face          ((t (:foreground ,highlight :background ,dim-highlight))))
   `(isearch                          ((t (:foreground "black" :background ,highlight :inverse-video nil))))
   `(isearch-lazy-highlight-face      ((t (:foreground ,text :background ,dim-highlight))))
   `(evil-search-highlight-persist-highlight-face
     ((t (:background nil :foreground nil :inherit isearch-lazy-highlight-face))))

   ))


;; *****************************************************************************************

(provide-theme 'v0)

;; Local Variables:
;; no-byte-compile: t
;; End:
