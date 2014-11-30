;;; hl-todo.el --- highlight TODO keywords

;; Copyright (C) 2013-2014  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20130310
;; Homepage: http://github.com/tarsius/hl-todo
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Hightlight TODO keywords.  There are many minor modes like it
;; but this one is mine.  It also happens to be simpler than the
;; alternatives.

;; For now at least -- I might extend it.  Or I might abandon it
;; in favor of one of the following -- so you might be better of
;; going straight for one of these:

;; - [[http://emacswiki.org/fic-ext-mode.el][fic-ext-mode]]
;; - [[https://github.com/lewang/fic-mode][fic-mode]]
;; - [[http://emacswiki.org/FixmeMode][fixme-mode]]
;; - [[https://github.com/rolandwalker/fixmee][fixmee]]
;; - see http://emacswiki.org/FixmeMode for more alternatives

;; If you like this you might also like [[https://github.com/tarsius/orglink][orglink]].

;;; Code:

(defgroup hl-todo nil
  "Highlight TODO keywords in comments."
  :group 'font-lock-extra-types)

(defface hl-todo
  '((t (:bold t :foreground "#cc9393")))
  "Face used to highlight TODO keywords."
  :group 'hl-todo)

(defcustom hl-todo-activate-in-modes '(emacs-lisp-mode)
  "Major modes in which `hl-todo-mode' should be activated.
This is used by `global-hl-todo-mode'."
  :group 'hl-todo
  :type '(repeat function))

(defvar hl-todo-keywords nil)

(defcustom hl-todo-keyword-faces
  '(("HOLD" . "#d0bf8f")
    ("TODO" . "#cc9393")
    ("NEXT" . "#dca3a3")
    ("THEM" . "#dc8cc3")
    ("PROG" . "#7cb8bb")
    ("OKAY" . "#7cb8bb")
    ("DONT" . "#5f7f5f")
    ("FAIL" . "#8c5353")
    ("DONE" . "#afd8af")
    ("FIXME" . "#cc9393")
    ("XXX"   . "#cc9393")
    ("XXXX"  . "#cc9393")
    ("???"   . "#cc9393"))
  "Faces used to highlight specific TODO keywords."
  :group 'hl-todo
  :type '(repeat (cons (string :tag "Keyword")
                       (choice :tag "Face   "
                               (string :tag "Color")
                               (sexp :tag "Face"))))
  :set (lambda (symbol value)
         (set-default symbol value)
         (setq hl-todo-keywords
               `((,(concat "\\_<\\("
                           (mapconcat 'car value "\\|")
                           "\\)\\_>")
                  (1 (hl-todo-get-face) t))))))

(defun hl-todo-get-face ()
  (let ((f (cdr (assoc (match-string 1) hl-todo-keyword-faces))))
    (if (stringp f) (list :inherit 'hl-todo :foreground f) f)))

;;;###autoload
(define-minor-mode hl-todo-mode
  "Highlight TODO tags in comments."
  :lighter ""
  :group 'hl-todo
  (if hl-todo-mode
      (font-lock-add-keywords  nil hl-todo-keywords 'append)
    (font-lock-remove-keywords nil hl-todo-keywords))
  (when (called-interactively-p 'any)
    (font-lock-fontify-buffer)))

;;;###autoload
(define-globalized-minor-mode global-hl-todo-mode
  hl-todo-mode turn-on-hl-todo-mode-if-desired)

(defun turn-on-hl-todo-mode-if-desired ()
  (when (apply 'derived-mode-p hl-todo-activate-in-modes)
    (hl-todo-mode 1)))

(provide 'hl-todo)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; hl-todo.el ends here
