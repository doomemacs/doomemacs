;;; tools/eval/autoload/eval.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +eval-display-results-in-popup (output &optional _source-buffer)
  "Display OUTPUT in a popup buffer."
  (let ((output-buffer (get-buffer-create "*doom eval*"))
        (origin (selected-window)))
    (with-current-buffer output-buffer
      (setq-local scroll-margin 0)
      (erase-buffer)
      (insert output)
      (goto-char (point-min))
      (if (fboundp '+word-wrap-mode)
          (+word-wrap-mode +1)
        (visual-line-mode +1)))
    (when-let (win (display-buffer output-buffer))
      (fit-window-to-buffer
       win (/ (frame-height) 2)
       nil (/ (frame-width) 2)))
    (select-window origin)
    output-buffer))

;;;###autoload
(defun +eval-display-results-in-overlay (output &optional source-buffer)
  "Display OUTPUT in a floating overlay next to the cursor."
  (require 'eros)
  (with-current-buffer (or source-buffer (current-buffer))
    (let* ((this-command #'+eval/buffer-or-region)
           (prefix eros-eval-result-prefix)
           (lines (split-string output "\n"))
           (prefixlen (length prefix))
           (len (+ (apply #'max (mapcar #'length lines))
                   prefixlen))
           (col (- (current-column) (window-hscroll)))
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

;;;###autoload
(defun +eval-region-as-major-mode (beg end &optional runner-major-mode)
  "Evaluate a region between BEG and END and display the output.

Evaluate as in RUNNER-MAJOR-MODE. If RUNNER-MAJOR-MODE is nil, use major-mode
of the buffer instead."
  (let ((load-file-name buffer-file-name)
        (load-true-file-name
         (or buffer-file-truename
             (if buffer-file-name
                 (file-truename buffer-file-name))))
        (runner-major-mode (or runner-major-mode major-mode)))
    (cond ((if (fboundp '+eval--ensure-in-repl-buffer)
               (ignore-errors
                 (get-buffer-window (or (+eval--ensure-in-repl-buffer)
                                        t))))
           (funcall (or (plist-get (cdr (alist-get runner-major-mode +eval-repls)) :send-region)
                        #'+eval/send-region-to-repl)
                    beg end))
          ((let (lang)
             (if-let* ((runner
                        (or (alist-get runner-major-mode +eval-runners)
                            (and (require 'quickrun nil t)
                                 (equal (setq
                                         lang (quickrun--command-key
                                               (buffer-file-name (buffer-base-buffer))))
                                        "emacs")
                                 (alist-get 'emacs-lisp-mode +eval-runners)))))
                 (funcall runner beg end)
               (let ((quickrun-option-cmdkey lang))
                 (quickrun-region beg end))))))))


;;
;;; Commands

(defvar quickrun-option-cmdkey)
;;;###autoload
(defun +eval/buffer ()
  "Evaluate the whole buffer."
  (interactive)
  (let ((quickrun-option-cmdkey (bound-and-true-p quickrun-option-cmdkey)))
    (if (or (assq major-mode +eval-runners)
            (and (fboundp '+eval--ensure-in-repl-buffer)
                 (ignore-errors
                   (get-buffer-window (or (+eval--ensure-in-repl-buffer)
                                          t))))
            (and (require 'quickrun nil t)
                 (equal (setq
                         quickrun-option-cmdkey
                         (quickrun--command-key
                          (buffer-file-name (buffer-base-buffer))))
                        "emacs")
                 (alist-get 'emacs-lisp-mode +eval-runners)))
        (if-let* ((buffer-handler (plist-get (cdr (alist-get major-mode +eval-repls)) :send-buffer)))
            (funcall buffer-handler)
          (+eval/region (point-min) (point-max)))
      (quickrun))))

;;;###autoload
(defun +eval/region (beg end)
  "Evaluate a region between BEG and END and display the output."
  (interactive "r")
  (+eval-region-as-major-mode beg end))

;;;###autoload
(defun +eval/line-or-region ()
  "Evaluate the current line or selected region."
  (interactive)
  (if (use-region-p)
      (call-interactively #'+eval/region)
    (+eval/region (line-beginning-position) (line-end-position))))

;;;###autoload
(defun +eval/buffer-or-region ()
  "Evaluate the region if it's active, otherwise evaluate the whole buffer.

If a REPL is open the code will be evaluated in it, otherwise a quickrun
runner will be used."
  (interactive)
  (call-interactively
   (if (use-region-p)
       #'+eval/region
     #'+eval/buffer)))

;;;###autoload
(defun +eval/region-and-replace (beg end)
  "Evaluation a region between BEG and END, and replace it with the result."
  (interactive "r")
  (let (lang)
    (cond
     ((or (eq major-mode 'emacs-lisp-mode)
          (and (require 'quickrun nil t)
               (equal (setq
                       lang (quickrun--command-key
                             (buffer-file-name (buffer-base-buffer))))
                      "emacs")))
      (kill-region beg end)
      (condition-case nil
          (prin1 (eval (read (current-kill 0)))
                 (current-buffer))
        (error (message "Invalid expression")
               (insert (current-kill 0)))))
     ((let ((quickrun-option-cmdkey lang))
        (quickrun-replace-region beg end))))))
