;;; tools/debugger/autoload/debugger.el -*- lexical-binding: t; -*-

(defvar +debugger--last nil)

(defun +debugger-list-for-dap ()
  (when (and (bound-and-true-p lsp-mode)
             (bound-and-true-p lsp--buffer-deferred)
             (require 'dap-mode nil t)
             dap-mode)
    (mapcar #'car dap-debug-template-configurations)))

(defun +debugger-list-for-realgud ()
  (cl-loop for (sym . plist) in +debugger--realgud-alist
           for sym-name = (symbol-name sym)
           for modes = (plist-get plist :modes)
           if (or (null modes) (apply #'derived-mode-p modes))
           collect sym))


(defun +debugger-list-available ()
  "TODO"
  (append (+debugger-list-for-dap)
          (+debugger-list-for-realgud)
          nil))


;;
;;; Interactive commands

;;;###autoload
(defun +debugger/start-last ()
  "Relaunch the last debugger session."
  (interactive)
  (unless +debugger--last
    (user-error "No last debugger to invoke"))
  (call-interactively +debugger--last))

;;;###autoload
(defun +debugger/start (arg)
  "Launch a debugger session.

Launches the last used debugger, if one exists. Otherwise, you will be prompted
for what debugger to use. If the prefix ARG is set, prompt anyway."
  (interactive "P")
  (if (or arg (null +debugger--last))
      (let ((debugger (intern-soft (completing-read "Start debugger: " (+debugger-list-available)))))
        (unless debugger
          (user-error "No debugging session to quit"))
        (unless (fboundp debugger)
          (user-error "Couldn't find debugger backend %S" debugger))
        (setq-local +debugger--last debugger)
        (if (assoc debugger dap-debug-template-configurations)
            (dap-debug debugger)
          (call-interactively debugger)))
    (+debugger/start-last)))

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
