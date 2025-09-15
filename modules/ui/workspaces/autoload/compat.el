;;; ui/workspaces/autoload/compat.el -*- lexical-binding: t; -*-
;;;###if (versionp! emacs-version < "30.1")

;;;###autoload
(defcustom tab-bar-tab-post-select-functions nil
  "List of functions to call after selecting a tab.
Two arguments are supplied: the previous tab that was selected before,
and the newly selected tab."
  :type '(repeat function)
  :group 'tab-bar
  :version "30.1")

;;;###autoload
(define-advice tab-bar-select-tab (:around (fn &optional tab-number))
  (let* ((tabs (funcall tab-bar-tabs-function))
         (from-index (tab-bar--current-tab-index tabs))
         (to-number (cond ((< tab-number 0) (+ (length tabs) (1+ tab-number)))
                          ((zerop tab-number) (1+ from-index))
                          (t tab-number)))
         (to-index (1- (max 1 (min to-number (length tabs)))))
         (minibuffer-was-active (minibuffer-window-active-p (selected-window)))
         (from-tab (tab-bar--tab))
         (to-tab (nth to-index tabs)))

    (funcall fn tab-number)
    (unless (eq from-index to-index)
      (run-hook-with-args 'tab-bar-tab-post-select-functions
                          from-tab to-tab)))
