;;; tools/eval/config.el -*- lexical-binding: t; -*-

(defgroup +eval nil
  "Tools and commands for evaluating code universally and managing REPLs."
  :group 'doom+)

(defcustom +eval-handler-functions
  '(+eval-with-repl-fn
    +eval-with-mode-handler-fn
    +eval-with-quickrun-fn)
  "A list of functions to execute when evaluating a region/buffer.

Stops at the first function to return non-nil. Each function takes three
arguments: a beginning position (int), an end position (int), and a symbol
(either `region' or `buffer') to hint at the scope of the evaluation.

Only affects `+eval/region', `+eval/buffer', and any other command that use
these them."
  :type 'hook
  :group '+eval)

(defcustom +eval-popup-min-lines 4
  "The output height threshold (inclusive) before output is displayed in a popup
buffer rather than an overlay on the line at point or the minibuffer."
  :type 'integer
  :group '+eval)


;;
;;; Config

;; Remove ellipsis when printing sexps in message buffer
(setq eval-expression-print-length nil
      eval-expression-print-level  nil)

;; These commands are drop-in replacements, but present an overlay/popup to
;; display the return value, and emit a backtrace if an error is encountered.
(global-set-key [remap eval-region] #'+eval/region)
(global-set-key [remap eval-buffer] #'+eval/buffer)

(set-popup-rule!
  (lambda (bufname _)
    (and (boundp '+eval-repl-plist)
         (buffer-local-value '+eval-repl-plist (get-buffer bufname))))
  :ttl (lambda (buf)
         (unless (plist-get +eval-repl-plist :persist)
           (when-let* ((process (get-buffer-process buf)))
             (set-process-query-on-exit-flag process nil)
             (kill-process process)
             (kill-buffer buf))))
  :size 0.25 :quit nil)


(after! quickrun
  (setq quickrun-focus-p nil)

  (set-popup-rule! "^\\*quickrun" :size 0.3 :ttl 0)

  (defadvice! +eval--quickrun-fix-evil-visual-region-a ()
    "Make `quickrun-replace-region' recognize evil visual selections."
    :override #'quickrun--outputter-replace-region
    (let ((output (buffer-substring-no-properties (point-min) (point-max))))
      (with-current-buffer quickrun--original-buffer
        (cl-destructuring-bind (beg . end)
            ;; Because `deactivate-mark', the function, was used in
            ;; `quickrun--region-command-common' instead of `deactivate-mark',
            ;; the variable, the selection is disabled by this point.
            (if (bound-and-true-p evil-local-mode)
                (cons evil-visual-beginning evil-visual-end)
              (cons (region-beginning) (region-end)))
          (delete-region beg end)
          (insert output))
        (setq quickrun-option-outputter quickrun--original-outputter))))

  (defadvice! +eval--quickrun-auto-close-a (&rest _)
    "Silently re-create the quickrun popup when re-evaluating."
    :before #'quickrun
    :before #'quickrun-region
    (when-let* ((win (get-buffer-window quickrun--buffer-name)))
      (let ((inhibit-message t))
        (quickrun--kill-running-process)
        (message ""))
      (delete-window win)))

  (add-hook! 'quickrun-after-run-hook
    (defun +eval-quickrun-shrink-window-h ()
      "Shrink the quickrun output window once code evaluation is complete."
      (when-let* ((win (get-buffer-window quickrun--buffer-name)))
        (with-selected-window win
          (let ((ignore-window-parameters t))
            (shrink-window-if-larger-than-buffer)))))
    (defun +eval-quickrun-scroll-to-bof-h ()
      "Ensures cursor is at beginning of output window when displayed."
      (when-let* ((win (get-buffer-window quickrun--buffer-name)))
        (with-selected-window win
          (goto-char (point-min))))))

  ;; HACK: Display evaluation results in an overlay at the end of the current
  ;;   line. If the output is more than `+eval-popup-min-lines' (4) lines long,
  ;;   it is displayed in a popup.
  (when (modulep! +overlay)
    (defadvice! +eval--show-output-in-overlay-a (fn)
      :filter-return #'quickrun--make-sentinel
      (lambda (process event)
        (funcall fn process event)
        (with-current-buffer quickrun--buffer-name
          (when (> (buffer-size) 0)
            (+eval-display-results
             (string-trim (buffer-string))
             quickrun--original-buffer)))))

    ;; HACK: Suppress quickrun's popup because we're using an overlay instead.
    (defadvice! +eval--inhibit-quickrun-popup-a (buf cb)
      :override #'quickrun--pop-to-buffer
      (setq quickrun--original-buffer (current-buffer))
      (save-window-excursion
        (with-current-buffer (pop-to-buffer buf)
          (setq quickrun-option-outputter #'ignore)
          (funcall cb))))

    ;; HACK: Without this, `+eval--inhibit-quickrun-popup-a' throws a
    ;;   window-live-p error because no window exists to be recentered!
    (advice-add #'quickrun--recenter :override #'ignore)))


(use-package! eros
  :when (modulep! +overlay)
  :hook (emacs-lisp-mode . eros-mode))
