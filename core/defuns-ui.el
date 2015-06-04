(eval-when-compile (require 'cl))

;;;###autoload
(defun narf:toggle-transparency ()
  (interactive)
  (let* ((alpha (frame-parameter nil 'alpha))
         (alpha-val (if (listp alpha) (car alpha) alpha)))
    (if (/= alpha-val 97)
        (set-frame-parameter nil 'alpha 100)
      (set-frame-parameter nil 'alpha 0))))

;;;###autoload
(defun narf:toggle-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

(defconst BIG-FONT (font-spec :family "Inconsolata" :size 18 :antialias t))
(defvar narf--big-mode nil)

;;;###autoload
(defun narf:toggle-big-mode ()
  (interactive)
  (if narf--big-mode
      (set-frame-font DEFAULT-FONT)
    (set-frame-font BIG-FONT))
  (setq narf--big-mode (not narf--big-mode)))
