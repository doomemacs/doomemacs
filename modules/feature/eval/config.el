;;; feature/repl/config.el

;; + Running inline code using `quickrun'
;; + REPLs using `repl-toggle'

(defvar doom-repl-buffer nil
  "The current REPL buffer.")

(defvar +eval-builders (make-hash-table :test 'equal)
  "A hash-table of plists, containing functions for building source code. Used
by `+eval/build', and filled with the `:build' setting")

;; remove ellipsis when printing sexp in message buffer
(setq eval-expression-print-length nil
      eval-expression-print-level  nil)

(def-setting! :repl (mode command)
  "Define a REPL for a mode. Takes the same arguements as `rtog/add-repl'."
  `(after! repl-toggle
     (rtog/add-repl ',mode ',command)))

(def-setting! :build (name mode pred-fn &optional build-fn)
  "Define a build command function (BUILD-FN) for major-mode MODE, called NAME
-- a symbol -- PRED-FN is a predicate function that determines this builder's
suitability for the current buffer."
  (unless build-fn
    (setq build-fn pred-fn
          pred-fn nil))
  `(puthash ',(cons name mode)
            (list :predicate ,pred-fn :fn ,build-fn)
            +eval-builders))

(def-setting! :eval (mode command)
  "Define a code evaluator for `quickrun'.

1. If MODE is a string and COMMAND is the string, MODE is a file regexp and
   COMMAND is a string key for an entry in `quickrun-file-alist'.
2. If MODE is not a string and COMMAND is a string, MODE is a major-mode symbol
   and COMMAND is a key; they will be registered in
   `quickrun--major-mode-alist'.
3. If MODE is not a string and COMMAND is a list, use `quickrun-add-command'. e.g.
   (quickrun-add-command MODE COMMAND :mode MODE)"
  (if (stringp command)
      `(after! quickrun
         (push ',(cons mode command)
               ,(if (stringp mode)
                    'quickrun-file-alist
                  'quickrun--major-mode-alist)))
    `(after! quickrun
       (quickrun-add-command
        ,(symbol-name mode)
        ',command :mode ',mode))))


;;
;; Packages
;;

(def-package! quickrun
  :commands (quickrun
             quickrun-region
             quickrun-with-arg
             quickrun-shell
             quickrun-compile-only
             quickrun-replace-region)
  :init
  (add-hook 'quickrun/mode-hook 'linum-mode)
  :config
  (set! :popup "*quickrun*" :size 10)

  ;; don't auto-focus quickrun windows. Shackle handles that for us.
  (setq quickrun-focus-p nil)

  (defun +repl*quickrun-close-popup (&optional _ _ _ _)
    "Allows us to silently re-run quickrun from within the quickrun buffer."
    (awhen (get-buffer-window quickrun/buffer-name)
      (let (message-log-max)
        (quickrun/kill-running-process)
        (message ""))
      (doom/popup-close it)))

  (defun +repl|quickrun-scroll-to-bof ()
    "Ensures window is scrolled to BOF on invocation."
    (with-selected-window (get-buffer-window quickrun/buffer-name)
      (goto-char (point-min))))

  ;;; Popup hacks
  (advice-add 'quickrun :before '+repl*quickrun-close-popup)
  (advice-add 'quickrun-region :before '+repl*quickrun-close-popup)
  ;; Ensures window is scrolled to BOF
  (add-hook 'quickrun-after-run-hook '+repl|quickrun-scroll-to-bof))


(def-package! repl-toggle
  :commands rtog/toggle-repl
  :preface (defvar rtog/mode-repl-alist nil)
  :config
  (set! :popup
    '(:custom (lambda (b &rest _)
                (when (and (featurep 'repl-toggle)
                           (string-prefix-p "*" (buffer-name (get-buffer b))))
                  (buffer-local-value 'repl-toggle-mode b))))
    :size 16)

  (map! :map repl-toggle-mode-map
        :ei "C-n" 'comint-next-input
        :ei "C-p" 'comint-previous-input
        :ei "<down>" 'comint-next-input
        :ei "<up>"   'comint-previous-input))

