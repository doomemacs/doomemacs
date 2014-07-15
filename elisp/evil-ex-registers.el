;;; evil-ex-registers.el --- Command to paste from register in ex mode

;; Author: INA Lintaro <tarao.gnn at gmail.com>
;; URL: http://github.com/tarao/evil-plugins
;; Version: 0.1
;; Keywords: evil, plugin

;; This file is NOT part of GNU Emacs.

;;; License:
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'evil)
(eval-when-compile (require 'cl))

(defalias 'evil-orig-get-register (symbol-function 'evil-get-register))

(defun evil-get-spec-register (register &optional noerror)
  "Return contents of REGISTER.
Signal an error if empty, unless NOERROR is non-nil.

Support some registers listed below in addition to
`evil-get-register'.
    the file name under the cursor
    the expanded file name under the cursor
    the word under the cursor
    the WORD under the cursor"
  (cond
   ((or (= register ?\C-f)  ; ^F the filename under the cursor
        (= register ?\C-p)) ; ^P the expanded filename under the cursor
    (let ((file (thing-at-point 'filename)))
      (or (and file (= register ?\C-p) (expand-file-name file)) file)))
   ((or (= register ?\C-w)  ; ^W the word under the cursor
        (= register ?\C-a)) ; ^A the WORD under the cursor
    (let* ((word (if (= register ?\C-a) #'evil-move-WORD #'evil-move-word))
           (range (evil-inner-object-range nil nil nil nil word)))
      (filter-buffer-substring (nth 0 range) (nth 1 range))))
   (t (evil-orig-get-register register noerror))))

(defun evil-ex-paste-from-register (&optional register)
  "Paste from REGISTER in command line."
  (interactive)
  (flet ((evil-get-register (register &optional noerror)
           (with-current-buffer evil-ex-current-buffer
             (evil-get-spec-register register noerror))))
    (if (called-interactively-p 'any)
        (call-interactively #'evil-paste-from-register)
      (evil-paste-from-register register))))

(provide 'evil-ex-registers)
;;; evil-ex-registers.el ends here
