;;; lang/scheme/autoload.el -*- lexical-binding: t; -*-

;; HACK `geiser' has poor autoload etiquette. It calls
;;      `geiser-activate-implementation' and `geiser-implementation-extension'
;;      in their autoloads files. Sure, these functions are autoloaded, but this
;;      needlessly (and unavoidably) pulls in the `geiser-impl' package (et co)
;;      when geiser-X's autoloads are read (i.e. at startup).
;;
;;      I rectify this by inlining calls to these two functions (and the
;;      `geiser-impl--add-to-alist' sub-call in
;;      `geiser-implementation-extension'), and autoloading the two variables
;;      they operate on. I do this from our autoloads file (which is
;;      byte-compiled and read at startup before package autoloads).
;; TODO At some point, PR this behavior upstream (but not verbatim!)
;;;###autoload (defvar geiser-active-implementations ())
;;;###autoload (defvar geiser-implementations-alist ())
;;;###autoload (eval-and-compile (dolist (sym '(geiser-impl--add-to-alist geiser-activate-implementation geiser-implementation-extension)) (put sym 'byte-optimizer 'byte-compile-inline-expand)))


(defvar calculate-lisp-indent-last-sexp)
;; Adapted from https://github.com/alezost/emacs-config/blob/master/utils/al-scheme.el#L76-L123
;;;###autoload
(defun +scheme-indent-function-a (indent-point state)
  "Advice to replace `scheme-indent-function'.

This function is the same as `scheme-indent-function' except it properly indents
property lists and names starting with 'default'."
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
             ;; NOTE looking-at -> looking-at-p
             (not (looking-at-p "\\sw\\|\\s_")))
        (progn
          ;; NOTE (if (not ...) (progn ...)) -> (unless ... ...)
          (unless (> (save-excursion (forward-line 1) (point))
                     calculate-lisp-indent-last-sexp)
            (goto-char calculate-lisp-indent-last-sexp)
            (beginning-of-line)
            (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t))
          (backward-prefix-chars)
          (current-column))
      ;; NOTE let -> let* & moved `method' def into let bindings
      (let* ((function (buffer-substring
                        (point) (progn (forward-sexp 1) (point))))
             (method (or (get (intern-soft function) 'scheme-indent-function)
                         (get (intern-soft function) 'scheme-indent-hook))))
        (cond ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        ;; NOTE string-match -> string-match-p
                        ;; NOTE The original regexp is "\\`def" but it will mess
                        ;;      up indentation with such names as 'default-...'.
                        (string-match-p "\\`def" function)))
               (lisp-indent-defform state indent-point))
              ;; NOTE Added this clause to handle alignment of keyword symbols
              ((and (null method)
                    (> (length function) 1)
                    ;; NOTE string-match -> string-match-p
                    (string-match-p "\\`:" function))
               (let ((lisp-body-indent 1))
                 (lisp-indent-defform state indent-point)))
              ((integerp method)
               (lisp-indent-specform method state indent-point normal-indent))
              (method
               (funcall method state indent-point normal-indent)))))))

;;;###autoload
(defun +scheme/open-repl ()
  "Open the Scheme REPL."
  (interactive)
  (call-interactively #'switch-to-geiser)
  (current-buffer))
