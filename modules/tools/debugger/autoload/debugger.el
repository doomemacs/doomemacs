;;; tools/debugger/autoload/debugger.el -*- lexical-binding: t; -*-

(defvar +debugger-last-configuration nil
  "Configuration of the last debugging session of buffer.")
(make-variable-buffer-local '+debugger-last-configuration)

(defun +debugger-get-configuration ()
  "Get last debugging configuration.

If in a project, returns the configuration of the last debugging session in the
project, if any. Else, returns the last debugging configuration of the current
buffer, if any."
  (if (projectile-project-p)
      (projectile-variable-get '+debugger-last-configuration)
    +debugger-last-configuration))

(defun +debugger-set-configuration (configuration)
  "Set the debugging configuration.

If in a project, sets the project's debugging session configuration. Else, sets
the debugging configuration of the current buffer."
  (if (projectile-project-p)
      (projectile-variable-put '+debugger-last-configuration configuration)
    (setq-local +debugger-last-configuration configuration)))

(defun +debugger-list-for-dap ()
  (when (and (bound-and-true-p lsp-mode)
             (bound-and-true-p lsp--buffer-deferred)
             (require 'dap-mode nil t)
             dap-mode)
    (--map (cons 'dap it)
           (-mapcat #'funcall dap-launch-configuration-providers))))

(defun +debugger-list-for-realgud ()
  (--map (cons 'realgud (list (symbol-name it)))
         (cl-loop for (sym . plist) in +debugger--realgud-alist
                  for sym-name = (symbol-name sym)
                  for modes = (plist-get plist :modes)
                  if (or (null modes) (apply #'derived-mode-p modes))
                  collect sym)))

;; Based on dap--completing-read and dap-debug
(defun +debugger-completing-read ()
  "Completing read for debug configuration.

Presents both dap and realgud configurations, and returns a list of the form
('dap ...) or ('realgud ...) containing the corresponding debug configuration
infromation."
  (let* ((configurations (append
                          (+debugger-list-for-dap)
                          (+debugger-list-for-realgud)))
         (result (--map (cons (cadr it) it) configurations))
         (completion (completing-read "Start debugger: " (-map 'car result) nil t)))
    (if (eq completion "")
        (user-error "No debugging configuration specified.")
      (let ((configuration (cdr (assoc completion result))))
        (if (eq (car configuration) 'dap)
            ;; get dap debugging arguments
            (let* ((debug-args (-> (cdr configuration)
                                   cl-rest
                                   copy-tree
                                   dap-variables-expand-in-launch-configuration))
                   (launch-args (or (-some-> (plist-get debug-args :type)
                                      (gethash dap--debug-providers)
                                      (funcall debug-args))
                                    (user-error "Have you loaded the `%s' specific dap package?"
                                                (or (plist-get debug-args :type)
                                                    (user-error "%s does not specify :type" debug-args))))))
              (cons 'dap launch-args))
          (cons 'realgud (intern (cadr configuration))))))))

;;
;;; Interactive commands

;;;###autoload
(defun +debugger/start-last ()
  "Relaunch the last debugger session."
  (interactive)
  (let ((configuration (+debugger-get-configuration)))
    (unless configuration
      (user-error "No last debugger%s to invoke"
                  (if (projectile-project-p)
                      " of this project"
                    "")))
    (let ((launch-args (cdr configuration)))
      (if (eq (car configuration) 'dap)
          ;; start dap configuration
          (if (functionp launch-args)
              (funcall launch-args #'dap-start-debugging-noexpand)
            (dap-start-debugging-noexpand launch-args))
        ;; else start realgud configuration:
        (call-interactively launch-args)))))

;;;###autoload
(defun +debugger/start (arg)
  "Launch a debugger session.

Launches the last used debugger, if one exists. Otherwise, you will be prompted
for what debugger to use. If the prefix ARG is set, prompt anyway."
  (interactive "P")
  (if (or arg (null (+debugger-get-configuration)))
      (+debugger-set-configuration (+debugger-completing-read)))
  (+debugger/start-last))

;;;###autoload
(defun +debugger/quit ()
  "Quit the active debugger, if any."
  (interactive)
  (cond ((and (fboundp 'dap--cur-session) (dap--cur-session))
         (dap-disconnect))
        ((and (fboundp 'realgud-get-cmdbuf) (realgud-get-cmdbuf))
         (let ((buf (realgud-get-cmdbuf)))
           (ignore-errors
             (call-interactively #'realgud:cmd-quit))
           (let (realgud-safe-mode)
             (kill-buffer buf))))
        ((user-error "No debugging session to quit"))))

;; TODO debugger breakpoint commands
;; ;;;###autoload
;; (defun +debugger/toggle-breakpoint ()
;;   (interactive)
;;   (user-error "not implemented yet"))

;; ;;;###autoload
;; (defun +debugger/next-breakpoint ()
;;   (interactive)
;;   (user-error "not implemented yet"))

;; ;;;###autoload
;; (defun +debugger/previous-breakpoint ()
;;   (interactive)
;;   (user-error "not implemented yet"))
