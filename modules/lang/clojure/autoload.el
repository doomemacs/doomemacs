;;; lang/clojure/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +clojure-cider-lookup-definition (identifier)
  "A lookup handler for `cider-mode'.

This is necessary to fix `cider-find-dwim's inability to capture the full symbol
at point."
  (cider-find-dwim identifier))


;;
;;; Commands

;;;###autoload
(defun +clojure/open-repl (&optional arg type)
  "Open a Cider REPL for clojure and return the buffer."
  (interactive "P")
  ;; TODO Better error handling
  ;; type is `clj' for clojure and `cljs' for clojurescript
  ;; ... with no type specified, assume `clj'.
  (let ((type (or type 'clj)))
    (if-let* ((buffer (cider-current-repl type)))
        (pop-to-buffer buffer)
      (let ((process (cond ((eq type 'clj) (cider-jack-in-clj arg))
                           ((eq type 'cljs) (cider-jack-in-cljs arg)))))
        (message "Starting CIDER server for the first time...")
        (while (and (process-live-p process)
                    (not (cider-current-repl type)))
          (sit-for 1))
        (message "Starting CIDER server for the first time...done")
        (pop-to-buffer (cider-current-repl type))))))

;;;###autoload
(defun +clojure/open-cljs-repl (&optional arg)
  "Open a Cider REPL for clojurescript and return the buffer."
  (interactive "P")
  (+clojure/open-repl arg 'cljs))

;;;###autoload
(defun +clojure/cider-switch-to-repl-buffer-and-switch-ns ()
  "TODO"
  (interactive)
  (cider-switch-to-repl-buffer t))
