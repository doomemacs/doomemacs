;;; completion/company/autoload.el -*- lexical-binding: t; -*-

;;;###autodef
(defun set-company-backend! (modes &rest backends)
  "Prepends BACKENDS (in order) to `company-backends' in MODES.

MODES should be one symbol or a list of them, representing major or minor modes.
This will overwrite backends for MODES on consecutive uses.

If the car of BACKENDS is nil, unset the backends for MODES.

Examples:

  (set-company-backend! 'js2-mode 'company-tide 'company-yasnippet)
  (set-company-backend! 'sh-mode
    '(company-shell :with company-yasnippet))
  (set-company-backend! 'js2-mode
    '(:separate company-irony-c-headers company-irony))
  (set-company-backend! 'sh-mode nil)"
  (declare (indent defun))
  (dolist (mode (doom-enlist modes))
    (let ((hook (intern (format "%s-hook" mode)))
          (fn   (intern (format "+company|init-%s" mode))))
      (cond ((null (car-safe backends))
             (remove-hook hook fn)
             (unintern fn nil))
            ((fset fn
                   (lambda ()
                     (when (or (eq major-mode mode)
                               (and (boundp mode) (symbol-value mode)))
                       (require 'company)
                       (make-local-variable 'company-backends)
                       (dolist (backend (reverse backends))
                         (cl-pushnew backend company-backends
                                     :test (if (symbolp backend) #'eq #'equal))))))
             (add-hook hook fn))))))

;; FIXME obsolete :company-backend
;;;###autoload
(def-setting! :company-backend (modes &rest backends)
  :obsolete set-company-backend!
  `(set-company-backend! ,modes ,@backends))

;;;###autoload
(defun +company/toggle-auto-completion ()
  "Toggle as-you-type code completion."
  (interactive)
  (require 'company)
  (setq company-idle-delay (unless company-idle-delay 0.2))
  (message "Auto completion %s"
           (if company-idle-delay "enabled" "disabled")))

;;;###autoload
(defun +company/complete ()
  "Bring up the completion popup. If only one result, complete it."
  (interactive)
  (require 'company)
  (when (and (company-manual-begin)
             (= company-candidates-length 1))
    (company-complete-common)))

;;;###autoload
(defun +company/dabbrev ()
  "Invokes `company-dabbrev-code' in prog-mode buffers and `company-dabbrev'
everywhere else."
  (interactive)
  (call-interactively
   (if (derived-mode-p 'prog-mode)
       #'company-dabbrev-code
     #'company-dabbrev)))

;;;###autoload
(defun +company/whole-lines (command &optional arg &rest ignored)
  "`company-mode' completion backend that completes whole-lines, akin to vim's
C-x C-l."
  (interactive (list 'interactive))
  (require 'company)
  (pcase command
    (`interactive (company-begin-backend '+company/whole-lines))
    (`prefix      (company-grab-line "^[\t\s]*\\(.+\\)" 1))
    (`candidates
     (all-completions
      arg
      (split-string
       (replace-regexp-in-string
        "^[\t\s]+" ""
        (concat (buffer-substring-no-properties (point-min) (line-beginning-position))
                (buffer-substring-no-properties (line-end-position) (point-max))))
       "\\(\r\n\\|[\n\r]\\)" t)))))

;;;###autoload
(defun +company/dict-or-keywords ()
  "`company-mode' completion combining `company-dict' and `company-keywords'."
  (interactive)
  (require 'company-dict)
  (require 'company-keywords)
  (let ((company-backends '((company-keywords company-dict))))
    (call-interactively #'company-complete)))

;;;###autoload
(defun +company/dabbrev-code-previous ()
  (interactive)
  (require 'company-dabbrev)
  (let ((company-selection-wrap-around t))
    (call-interactively #'+company/dabbrev)
    (company-select-previous-or-abort)))
