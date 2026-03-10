;;; tools/eval/autoload/eval.el -*- lexical-binding: t; -*-

(defvar quickrun-option-cmdkey)
(defvar eros-overlays-use-font-lock)


;;
;;; Library

;;;###autoload
(defun +eval-display-results-in-popup (output &optional _source-buffer)
  "Display OUTPUT in a popup buffer at the bottom of the screen."
  (let ((output-buffer (get-buffer-create "*doom eval*")))
    (with-current-buffer output-buffer
      (setq-local scroll-margin 0)
      (erase-buffer)
      (save-excursion (insert output))
      (if (fboundp '+word-wrap-mode)
          (+word-wrap-mode +1)
        (visual-line-mode +1)))
    (when-let* ((win (display-buffer output-buffer)))
      (fit-window-to-buffer win (/ (frame-height) 2)
                            nil (/ (frame-width) 2)))
    output-buffer))

;;;###autoload
(defun +eval-display-results-in-overlay (output &optional source-buffer)
  "Display OUTPUT in a floating overlay next to or below the cursor."
  (require 'eros)
  (with-current-buffer (or source-buffer (current-buffer))
    (let* ((this-command #'+eval/buffer-or-region)
           (prefix eros-eval-result-prefix)
           (lines (split-string output "\n"))
           (prefixlen (length prefix))
           (len (+ (apply #'max (mapcar #'length lines))
                   prefixlen))
           (next-line? (or (cdr lines)
                           (< (- (window-width)
                                 (save-excursion (goto-char (line-end-position))
                                                 (- (current-column)
                                                    (window-hscroll))))
                              len)))
           (pad (if next-line?
                    (+ (window-hscroll) prefixlen)
                  0))
           eros-overlays-use-font-lock)
      (eros--make-result-overlay
          (concat (make-string (max 0 (- pad prefixlen)) ?\s)
                  prefix
                  (string-join lines (concat hard-newline (make-string pad ?\s))))
        :where (if next-line?
                   (line-beginning-position 2)
                 (line-end-position))
        :duration eros-eval-result-duration
        :format "%s"))))

;;;###autoload
(defun +eval-display-results (output &optional source-buffer)
  "Display OUTPUT in an overlay or a popup buffer."
  (funcall (if (or current-prefix-arg
                   (with-temp-buffer
                     (insert output)
                     (or (>= (count-lines (point-min) (point-max))
                             +eval-popup-min-lines)
                         (>= (string-width
                              (buffer-substring (point-min)
                                                (save-excursion
                                                  (goto-char (point-min))
                                                  (line-end-position))))
                             (window-width))))
                   (not (require 'eros nil t)))
               #'+eval-display-results-in-popup
             #'+eval-display-results-in-overlay)
           output source-buffer)
  output)


;;
;;; Eval handlers

;;;###autoload
(defun +eval-with-mode-handler-fn (beg end &optional _type mode)
  "Evaluate the selection/buffer using a mode appropriate handler.

Uses whatever handler's been registered for MODE (or the current major-mode)
with `set-eval-handler!'."
  (when-let* ((fn (alist-get (or mode major-mode) +eval-handler-alist)))
    (funcall fn beg end)))

;;;###autoload
(defun +eval-with-quickrun-fn (beg end &optional type)
  "Evaluate the region or buffer with `quickrun'."
  (when (require 'quickrun nil t)
    (pcase type
      (`buffer (quickrun))
      (`region (quickrun-region beg end))
      (`replace (quickrun-replace-region beg end)))
    t))


;;
;;; Commands

;;;###autoload
(defun +eval/buffer ()
  "Evaluate the whole buffer and display the output.

See `+eval-handler-functions' for order of backends this uses. By default, falls
back to `quickrun'."
  (interactive)
  (run-hook-with-args-until-success
   '+eval-handler-functions (point-min) (point-max) 'buffer))

;;;###autoload
(defun +eval/region (beg end)
  "Evaluate a region between BEG and END and display the output.

If a REPL is open, code will be executed there, otherwise mode-specific handlers
will be used, falling back to `quickrun' otherwise.

See `+eval-handler-functions' for order of backends this uses. By default, falls
back to `quickrun'."
  (interactive "r")
  (run-hook-with-args-until-success
   '+eval-handler-functions beg end 'region))

;;;###autoload
(defun +eval/line-or-region ()
  "Evaluate the current line or selected region.

If a REPL is open, code will be executed there, otherwise mode-specific handlers
will be used, falling back to `quickrun' otherwise."
  (interactive)
  (if (use-region-p)
      (call-interactively #'+eval/region)
    (+eval/region (pos-bol) (pos-eol))))

;;;###autoload
(defun +eval/buffer-or-region ()
  "Execute `+eval/region' if a selection is active, otherwise `+eval/buffer'."
  (interactive)
  (call-interactively
   (if (doom-region-active-p)
       #'+eval/region
     #'+eval/buffer)))

;;;###autoload
(defun +eval/region-and-replace (beg end)
  "Evaluate a region between BEG and END, and replace it with the result.

Uses `quickrun', unless in an `emacs-lisp-mode' buffer, in which case uses the
return value of `eval'."
  (interactive "r")
  (if (not (derived-mode-p 'emacs-lisp-mode))
      (quickrun-replace-region beg end)
    (kill-region beg end)
    (condition-case nil
        (prin1 (eval (read (current-kill 0)))
               (current-buffer))
      (error (message "Invalid expression")
             (insert (current-kill 0))))))

;;; eval.el ends here
