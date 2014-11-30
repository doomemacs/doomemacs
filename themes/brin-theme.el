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
;; Brin :- An Emacs port of the Space Grey ST2 theme
;;
;; *****************************************************************************************

;;
;; Modified by Henrik Lissner <http://github.com/hlissner/emacs.d>
;;

(unless (>= 24 emacs-major-version)
  (error "requires Emacs 24 or later."))

(deftheme brin "Space Grey theme for Emacs")

  (custom-theme-set-variables 'brin)

  (let ((background       "#222222")
        ;; (gutters          "#262E34")
        (gutters          "#1f252a")
        (gutter-fg        "#55616A")
        (gutters-active   "#2e363c")
        (linum            "#1e262c")
        ; (background     "#11141c")
        ; (gutters        "#343d46")
        ; (gutter-fg      "#65737e")
        ; (gutters-active "#4f5b66")
        (builtin          "#d08770")
        (foreground       "#c0c5ce")
        (invisibles       "#65737e")
        (lineHighlight    "#353539")
        (selection        "#4f5b66")
        (text             "#c0c5ce")
        (comments         "#65737e")
        (punctuation      "#c0c5ce")
        (delimiters       "#c0c5ce")
        (operators        "#c0c5ce")
        (keywords         "#b48ead")
        (variables        "#CBECFF")
        (functions        "#8fa1b3")
        (methods          "#8fa1b3")
        (strings          "#a3be8c")
        (constants        "#d08770")
        (white            "#ffffff")

        (git-modified     "#B4924E")
        (git-added        "#91E331")
        ;; (git-added        "#55811D")
        (git-deleted      "#A12121")
        )

  (custom-theme-set-faces
   'brin

;; Default colors
;; *****************************************************************************************

   `(default              ((t (:foreground ,text :background ,background) )))
   `(hl-line              ((t (:background ,lineHighlight) )))
   `(region               ((t (:background ,selection) )))
   `(cursor               ((t (:background ,white) )))
   `(fringe               ((t (:background ,background :foreground ,white) )))
   `(linum                ((t (:background ,background :foreground ,gutter-fg) )))

   `(vertical-border      ((t (:foreground ,gutters-active) )))

   `(mode-line            ((t (:foreground ,white
                               :background ,gutters-active
                               :box (:line-width 3 :color ,gutters-active) ))))

   `(mode-line-inactive       ((t (:foreground ,gutter-fg
                               :background ,background
                               :box (:line-width 3 :color ,gutters) ))))

   `(mode-line-modified-face           ((t (:foreground ,builtin))))

   ;; `(highlight-indentation-face                   ((t (:background ,linum) )))
   ;; `(highlight-indentation-current-column-face    ((t (:background ,gutters-active) )))

   `(flyspell-incorrect   ((t (:underline "#ff5555" :inherit unspecified))))

   `(helm-source-header       ((t (:background ,gutters-active :foreground ,strings :weight bold :height 1.0))))
   `(helm-selection       ((t (:background ,selection))))

;; Font lock faces
;; *****************************************************************************************

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

   `(git-gutter+-modified              ((t (:foreground ,git-modified))))
   `(git-gutter+-added                 ((t (:foreground ,git-added))))
   `(git-gutter+-deleted               ((t (:foreground ,git-deleted))))

;; js2-mode
;; *****************************************************************************************

   `(js2-function-param                ((t (:foreground ,variables))))
   `(js2-jsdoc-tag                     ((t (:foreground ,comments :weight bold :bold t))))


;; *****************************************************************************************

   `(persp-selected-face               ((t (:foreground ,builtin))))

   ))


;; *****************************************************************************************

(provide-theme 'brin)

;; Local Variables:
;; no-byte-compile: t
;; End:
