;;; ui/modeline/autoload/settings.el -*- lexical-binding: t; -*-

(defvar +modeline--alist nil)

(defun +modeline--segment-active-p (segment xs)
  (cond ((null xs) nil)
        ((listp xs)
         (or (+modeline--segment-active-p segment (car xs))
             (+modeline--segment-active-p segment (cdr xs))))
        ((eq xs segment))))

;;;###autoload
(defun +modeline-segment-active-p (segment)
  (or (+modeline--segment-active-p segment +modeline-format-left)
      (+modeline--segment-active-p segment +modeline-format-right)))

;;;###autodef
(defun def-modeline-format! (name left &optional right)
  "Define a preset modeline format by name.

NAME is a symbol. The convention is to use keywords for global formats, like
:main or :project, but to use regular symbols for buffer-local formats, like
'twitter and 'pdf.

LEFT and RIGHT are lists that assume the same structure as `mode-line-format',
and make up the mode-line in two parts, separated by variable-width space, to
keep them left and right aligned respectively."
  (setf (alist-get name +modeline--alist) (list left right)))

;;;###autodef
(defmacro def-modeline-segment! (name &rest rest)
  "TODO"
  (declare (doc-string 2))
  (let ((docstring (if (and (stringp (car rest)) (cdr rest)) (pop rest)))
        body)
    (macroexp-progn
     (if (not (keywordp (car rest)))
         (append `((defvar-local ,name nil ,docstring)
                   (put ',name 'risky-local-variable t))
                 (if (or (stringp (car rest))
                         (memq (car (car-safe rest)) '(:eval :propertize)))
                     `((setq-default ,name ,(car rest)))
                   (let ((fn (intern (format "+modeline--%s" name))))
                     `((fset ',fn (lambda () ,@rest))
                       (byte-compile ',fn)
                       (setq-default ,name (quote (:eval (,fn))))))))
       ;; isolate body
       (setq body rest)
       (while (keywordp (car body))
         (setq body (cddr body)))
       ;;
       (cl-destructuring-bind (&key init faces on-hooks on-set &allow-other-keys)
           rest
         (let ((realvar (if (and body faces)
                            (intern (format "+modeline--var-%s" name))
                          name)))
           (append (when body
                     (if (or on-hooks on-set)
                         (let ((setterfn    (intern (format "+modeline--set-%s" name)))
                               (varsetterfn (intern (format "+modeline--setvar-%s" name))))
                           (append `((fset ',setterfn
                                           (lambda (&rest _)
                                             (when (+modeline-segment-active-p ',name)
                                               (setq-local ,realvar ,(macroexp-progn body)))))
                                     (byte-compile ',setterfn))
                                   (mapcar (lambda (hook) `(add-hook ',hook #',setterfn))
                                           on-hooks)
                                   (when on-set
                                     `((fset ',varsetterfn
                                             (lambda (sym val op where)
                                               (and (eq op 'set) where
                                                    (with-current-buffer where
                                                      (set sym val)
                                                      (,setterfn)))))
                                       ,@(mapcan (lambda (var) `((add-variable-watcher ',var #',varsetterfn)))
                                                 on-set)))))
                       (setq init `(quote (:eval ,(macroexp-progn body))))
                       nil))
                   (if (eq realvar name)
                       `((defvar-local ,name nil ,docstring)
                         (setq-default ,name ,init))
                     `((defvar-local ,realvar ,init)
                       (defvar-local ,name nil ,docstring)
                       (setq-default
                        ,name '(:eval (cond ((active) ,realvar)
                                            (,realvar (substring-no-properties ,realvar)))))))
                   `((put ',name 'risky-local-variable t)))))))))

;;;###autodef
(defun set-modeline! (name &optional default)
  "Replace the current buffer's modeline with a preset mode-line format defined
with `def-modeline-format!'.

If DEFAULT is non-nil, make it the default mode-line for all buffers."
  (cl-check-type name symbol)
  (let ((modeline (cdr (assq name +modeline--alist))))
    (unless modeline
      (error "The %s modeline format does not exist" name))
    (if default
        (setq-default +modeline-format-left  `("" ,@(car  modeline))
                      +modeline-format-right `("" ,@(cadr modeline)))
      (setq +modeline-format-left  `("" ,@(car  modeline))
            +modeline-format-right `("" ,@(cadr modeline))))
    (force-mode-line-update)))
