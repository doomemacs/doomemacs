;;; message.el

(defconst doom-ansi-fg
  '((reset   . 0)
    (black   . 30)
    (red     . 31)
    (green   . 32)
    (yellow  . 33)
    (blue    . 34)
    (magenta . 35)
    (cyan    . 36)
    (white   . 37))
  "List of text colors.")

(defconst doom-ansi-bg
  '((on-black   . 40)
    (on-red     . 41)
    (on-green   . 42)
    (on-yellow  . 43)
    (on-blue    . 44)
    (on-magenta . 45)
    (on-cyan    . 46)
    (on-white   . 47))
  "List of colors to draw text on.")

(defconst doom-ansi-fx
  '((bold       . 1)
    (dark       . 2)
    (italic     . 3)
    (underscore . 4)
    (blink      . 5)
    (rapid      . 6)
    (contrary   . 7)
    (concealed  . 8)
    (strike     . 9))
  "List of styles.")

;;;###autoload
(defmacro ansi-format! (message &rest args)
  "An alternative to `format' that strips out ANSI codes if used in an
interactive session."
  `(cl-flet*
       (,@(mapcar
           (lambda (ansi)
             `(,(car ansi)
               (lambda (message &rest args)
                 (apply 'doom--ansi-apply ,(cdr ansi) message args))))
           (append doom-ansi-fg doom-ansi-bg doom-ansi-fx))
        (color (symbol-function 'doom--ansi-apply)))
     (format ,message ,@args)))

;;;###autoload
(defmacro ansi-message! (message &rest args)
  "An alternative to `message' that strips out ANSI codes if used in an
interactive session."
  `(message (ansi-format! ,message ,@args)))

(defun doom--ansi-apply (code format &rest args)
  (if noninteractive
      (format "\e[%dm%s\e[%sm"
              (if (numberp code)
                  code
                (cdr (or (assq code doom-ansi-fg)
                         (assq code doom-ansi-bg)
                         (assq code doom-ansi-fx))))
              (apply 'format format args) 0)
    (apply 'format format args)))


;; --- DOOM message buffer -----------------------------------------------------

;; TODO
;; (defun doom-message (message &rest args) (interactive))
