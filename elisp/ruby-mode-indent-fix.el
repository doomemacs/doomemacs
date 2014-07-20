;;; ruby-mode-indent-fix.el ---

;; this file is not part of Emacs

;; Copyright (C) 2012 Le Wang
;; Author: Le Wang
;; Maintainer: Le Wang
;; Description:
;; Author: Le Wang
;; Maintainer: Le Wang

;; Created: Sun Feb 26 23:27:17 2012 (+0800)
;; Version: 0.1
;; Last-Updated: Mon Mar 26 11:23:48 2012 (+0800)
;;           By: Le Wang
;;     Update #: 29
;; URL:
;; Keywords:
;; Compatibility:

;;; Installation:

;; (eval-after-load "ruby-mod" '(require 'ruby-mode-indent-fix))
;;
;;

;;; Commentary:

;; Fix some indentation issues with ruby-mode with advices.
;;
;; Based on work by Dmitry Gutov(dgutov)
;;   - http://stackoverflow.com/a/7622971/903943 and
;;   - https://gist.github.com/1274520

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

(eval-when-compile (require 'cl))

(provide 'ruby-mode-indent-fix)
(require 'autopair)


(defvar ruby--paren-closings-regex
  "[])}\"']"
  "regex matching closing paren or string delimiter.")

;; We make this advice around to avoid unnecessary buffer modifications.

(defadvice ruby-indent-line (around fix-closing-paren activate)
  "indent closing paren to line up properly.

i.e.

    foo_function( {:a => 'foo',
                   :b => 'bar'
                  }
                )

Note that the closing paren is vertically aligned with the opening paren.

note: `ruby-deep-indent-paren' has to be enabled for this to work."
  (let ((column (current-column))
        indent)
    (when ruby-deep-indent-paren
      (save-excursion
        (back-to-indentation)
        (let ((state (syntax-ppss)))
          (when (and (or (memq (autopair-find-pair (char-after)) ruby-deep-indent-paren)
                         (and (eq (char-after) ?\})
                              (eq 'brace (ruby--point-in-braced-proc))))
                     (not (zerop (car state))))
            (goto-char (cadr state))
            (setq indent (current-column))))))
    (if indent
        (indent-line-to indent)
      ad-do-it)))

(defun ruby--indent-before-all-sexps ()
  "
1. search backwards for a closing delimiter ON THIS LINE, then
   find the matching opening

2. if found, recurse, else the point is at a place we don't need
   to worry about sexps.
"
  (if (re-search-backward ruby--paren-closings-regex (point-at-bol) t)
      (let ((ppss (syntax-ppss))
            beg)
        (goto-char (match-beginning 0))
        (cond ((setq beg (nth 1 ppss))  ; brace
               (goto-char beg))
              ((nth 3 ppss)             ; string
               (goto-char (nth 8 ppss))))
        (ruby--indent-before-all-sexps))))

(defun ruby--point-in-braced-proc ()
  "returns 'proc if point is in braces where starting bracs is EOL or followed by arg-list

i.e.

    arr.each { |foo|
      // do stuff
    }

or

    1.times {
      // do stuff
    }
returns 'brace if point in brace

return nil otherwise
"
  (save-excursion
    (let ((ppss (syntax-ppss))
          beg)
      (cond ((nth 3 ppss)             ; string
             nil)
            ((setq beg (nth 1 ppss))  ; brace
             (goto-char beg)
             (if (looking-at-p "{[\t ]*\\(?:$\\||\\)")
                 'proc
               (when (looking-at-p "{")
                 'brace)))))))

(defadvice ruby-indent-line (around line-up-args activate)
  "indent new line after comma at EOL properly:

i.e.

    foo_function a_param,
                 b_param,
                 c_param

Note that all params line up after the function.
"
  (let (indent ppss)
    (save-excursion
      (back-to-indentation)
      (skip-chars-backward " \t\n")
      (setq ppss (syntax-ppss))
      ;; check for inside comment, string, or inside braces
      (when (and (eq ?, (char-before))
                 (not (memq (syntax-ppss-context ppss) '(comment string)))
                 (zerop (car ppss)))
        (ruby--indent-before-all-sexps)
        (back-to-indentation)
        (if (save-excursion
              (skip-chars-backward " \t\n")
              (eq (char-before) ?,))
            (setq indent (current-column))
          (skip-syntax-forward "w_.")
          (skip-chars-forward " ")
          ;; if the first symbol on the line is followed, by a comma, then this
          ;; line must be a continuation
          (setq indent (current-column)))))
    (if indent
        (indent-line-to indent)
      ad-do-it)))

;; (defadvice ruby-indent-line (around indent-no-brace-args activate)
;;   "indent new line after comma at EOL properly:

;; i.e.

;;     foo_function a_param,
;;       b_param,
;;       c_param

;; Note that all params line up after the function."
;;   (let ((res (ruby--point-in-braced-proc)))
;;     (cond ((eq 'brace res)
;;            (let ((ruby-deep-indent-paren '(?\[ ?\( ?\{ t)))
;;              ad-do-it))
;;           (t
;;            ad-do-it))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ruby-mode-indent-fix.el ends here
