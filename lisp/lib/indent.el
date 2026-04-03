;;; lisp/lib/indent.el -*- lexical-binding: t; -*-

;; REVIEW: Move this out of autoloads in v3
;;;###autoload
(progn
  (defvar doom-indent-excluded-modes '(special-mode org-mode)
    "Modes not to no-op `doom-set-indent' in.")

  (defun set-indent-vars! (modes vars)
    "Register VARS as the tab-width proxy variables for each of major MODES.

`tab-width' and `standard-indent' will be set to match the values of these
variables in their respective buffers. Failing that, a best-guess effort will be
made to find an appropriate variable to use.

If MODES is `t', VARS will be made to match `tab-width' in all major modes.
Both MODES and VARS can be a single symbol or a list thereof.

Note that it's not necessary to register indent variables if they end in
MODE-ts-mode-indent-offset, MODE-indent-offset, MODE-indent-level,
MODE-tab-width, or MODE-basic-offset. It is also not necessary if you have
`dtrt-indent' installed and a variable association exists in
`dtrt-indent-hook-mapping-list'."
    (let ((vars (ensure-list vars)))
      (if (eq modes t)
          (put 'tab-width 'indent-vars vars)
        (dolist (mode (ensure-list modes))
          (put mode 'indent-vars vars)))))

  (defun doom-indent-var-for-mode (mode)
    "Try to guess MODE's indent offset variable based on heuristics.

Modes for whom this fails need to have their indent vars manually registered
with `set-indent-vars!'."
    (unless (apply #'provided-mode-derived-p mode doom-indent-excluded-modes)
      (let* ((mode* (symbol-name mode))
             (mode** (cl-loop for s in '("-mode" "-base" "-ts")
                              do (setq mode* (string-remove-suffix s mode*))
                              finally return mode*))
             (suffixes `(,@(if (string-suffix-p "-ts-mode" mode*)
                               '(-ts-mode-indent-offset
                                 -ts-indent-offset))
                         -indent-offset
                         -indent-level
                         -tab-width
                         -basic-offset)))
        (cl-loop for suffix in suffixes
                 for sym = (intern-soft (format "%s%s" mode** suffix))
                 if (and sym (boundp sym))
                 return sym))))

  (defun doom-indent-vars-for-mode (mode)
    "Try to auto-detect the indent variables for major MODE."
    (unless (eq mode 'fundamental-mode)
      (or (get mode 'indent-vars)
          (when (boundp 'dtrt-indent-hook-mapping-list)
            (ensure-list
             (nth 2 (assq major-mode dtrt-indent-hook-mapping-list))))
          ;; HACK: Best guess for modes that don't have registered indent
          ;;   variables. This handles the majority of them. `set-indent-vars!'
          ;;   is necessary for everything else.
          (ensure-list (doom-indent-var-for-mode mode)))))

  (defun doom-set-indent (&optional width)
    "Ensure tab-width reflects the local major mode's indent variable."
    (when-let* ((vars (doom-indent-vars-for-mode major-mode))
                (width* (or width
                            (cl-loop for v in vars
                                     if (boundp v)
                                     if (symbol-value v)
                                     return it))))
      (doom-log 2 "doom-set-indent: %S = %S" `(tab-width standard-indent ,@vars) width*)
      (setq-local tab-width width*
                  standard-indent width*)
      (dolist (var (append (get 'tab-width 'indent-vars) vars))
        (set (make-local-variable var) width*))))

  (add-hook 'change-major-mode-after-body-hook #'doom-set-indent -100)

  (set-indent-vars! '(lisp-mode lisp-interaction-mode lisp-data-mode)
                    'lisp-body-indent)
  (set-indent-vars! 'ps-mode 'ps-mode-tab))


;;
;;; Commands

;;;###autoload
(defun doom/set-indent-width (width)
  "Change the indentation size to WIDTH of the current buffer.

This relies on several heuristics to work. If "
  (interactive
   (list (if (integerp current-prefix-arg)
             current-prefix-arg
           (read-number "New indent size: "))))
  (doom-set-indent width)
  (when (bound-and-true-p dtrt-indent-original-indent)
    (setq dtrt-indent-original-indent nil))
  (message "Changed buffer's indent-size to %d" width))

;;;###autoload
(defun doom/toggle-indent-style ()
  "Switch between tabs and spaces indentation style in the current buffer."
  (interactive)
  (setq indent-tabs-mode (not indent-tabs-mode))
  (message "Indent style changed to %s" (if indent-tabs-mode "tabs" "spaces")))

;;;###autoload
(defun doom/retab (arg &optional beg end)
  "Converts tabs-to-spaces or spaces-to-tabs within BEG and END (defaults to
buffer start and end, to make indentation consistent. Which it does depends on
the value of `indent-tab-mode'.

If ARG (universal argument) is non-nil, retab the current buffer using the
opposite indentation style."
  (interactive "P\nr")
  (unless (and beg end)
    (setq beg (point-min)
          end (point-max)))
  (let ((indent-tabs-mode (if arg (not indent-tabs-mode) indent-tabs-mode)))
    (if indent-tabs-mode
        (tabify beg end)
      (untabify beg end))))

(provide 'doom-lib '(indent))
;;; indent.el ends here
