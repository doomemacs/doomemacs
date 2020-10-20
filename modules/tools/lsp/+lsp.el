;;; tools/lsp/+lsp.el -*- lexical-binding: t; -*-

(defvar +lsp-company-backends 'company-capf
  "The backends to prepend to `company-backends' in `lsp-mode' buffers.
Can be a list of backends; accepts any value `company-backends' accepts.")


;;
;;; Packages

(use-package! lsp-mode
  :commands lsp-install-server
  :init
  ;; Don't touch ~/.emacs.d, which could be purged without warning
  (setq lsp-session-file (concat doom-etc-dir "lsp-session")
        lsp-server-install-dir (concat doom-etc-dir "lsp/"))
  ;; Don't auto-kill LSP server after last workspace buffer is killed, because I
  ;; will do it for you, after `+lsp-defer-shutdown' seconds.
  (setq lsp-keep-workspace-alive nil)

  ;; NOTE I tweak LSP's defaults in order to make its more expensive or imposing
  ;;      features opt-in. Some servers implement these poorly and, in most
  ;;      cases, it's safer to rely on Emacs' native mechanisms (eldoc vs
  ;;      lsp-ui-doc, open in popup vs sideline, etc).

  ;; Disable features that have great potential to be slow.
  (setq lsp-enable-folding nil
        lsp-enable-text-document-color nil)
  ;; Reduce unexpected modifications to code
  (setq lsp-enable-on-type-formatting nil)

  ;; Let doom bind the lsp keymap.
  (when (featurep! :config default +bindings)
    (setq lsp-keymap-prefix nil))

  :config
  (pushnew! doom-debug-variables 'lsp-log-io 'lsp-print-performance)

  (setq lsp-intelephense-storage-path (concat doom-cache-dir "lsp-intelephense/")
        lsp-vetur-global-snippets-dir (expand-file-name "vetur"
                                                        (or (bound-and-true-p +snippets-dir)
                                                            (concat doom-private-dir "snippets/")))
        lsp-clients-emmy-lua-jar-path (concat lsp-server-install-dir "EmmyLua-LS-all.jar")
        lsp-xml-jar-file              (concat lsp-server-install-dir "org.eclipse.lsp4xml-0.3.0-uber.jar")
        lsp-groovy-server-file        (concat lsp-server-install-dir "groovy-language-server-all.jar"))

  (set-popup-rule! "^\\*lsp-help" :size 0.35 :quit t :select t)
  (set-lookup-handlers! 'lsp-mode :async t
    :documentation #'lsp-describe-thing-at-point
    :definition #'lsp-find-definition
    :implementations #'lsp-find-implementation
    :type-definition #'lsp-find-type-definition
    :references #'lsp-find-references)

  (defadvice! +lsp--respect-user-defined-checkers-a (orig-fn &rest args)
    "Ensure user-defined `flycheck-checker' isn't overwritten by `lsp'."
    :around #'lsp-diagnostics-flycheck-enable
    (if flycheck-checker
        (let ((old-checker flycheck-checker))
          (apply orig-fn args)
          (setq-local flycheck-checker old-checker))
      (apply orig-fn args)))

  (add-hook! 'lsp-mode-hook
    (defun +lsp-display-guessed-project-root-h ()
      "Log what LSP things is the root of the current project."
      ;; Makes it easier to detect root resolution issues.
      (when-let (path (buffer-file-name (buffer-base-buffer)))
        (if-let (root (lsp--calculate-root (lsp-session) path))
            (lsp--info "Guessed project root is %s" (abbreviate-file-name root))
          (lsp--info "Could not guess project root."))))
    #'+lsp-optimization-mode)

  (add-hook! 'lsp-completion-mode-hook
    (defun +lsp-init-company-backends-h ()
      (when lsp-completion-mode
        (set (make-local-variable 'company-backends)
             (cons +lsp-company-backends
                   (remove +lsp-company-backends
                           (remq 'company-capf company-backends)))))))

  (defvar +lsp--deferred-shutdown-timer nil)
  (defadvice! +lsp-defer-server-shutdown-a (orig-fn &optional restart)
    "Defer server shutdown for a few seconds.
This gives the user a chance to open other project files before the server is
auto-killed (which is a potentially expensive process). It also prevents the
server getting expensively restarted when reverting buffers."
    :around #'lsp--shutdown-workspace
    (if (or lsp-keep-workspace-alive
            restart
            (null +lsp-defer-shutdown)
            (= +lsp-defer-shutdown 0))
        (prog1 (funcall orig-fn restart)
          (+lsp-optimization-mode -1))
      (when (timerp +lsp--deferred-shutdown-timer)
        (cancel-timer +lsp--deferred-shutdown-timer))
      (setq +lsp--deferred-shutdown-timer
            (run-at-time
             (if (numberp +lsp-defer-shutdown) +lsp-defer-shutdown 3)
             nil (lambda (workspace)
                   (with-lsp-workspace workspace
                     (unless (lsp--workspace-buffers workspace)
                       (let ((lsp-restart 'ignore))
                         (funcall orig-fn))
                       (+lsp-optimization-mode -1))))
             lsp--cur-workspace)))))


(use-package! lsp-ui
  :defer t
  :config
  (setq lsp-ui-doc-max-height 8
        lsp-ui-doc-max-width 35
        lsp-ui-sideline-ignore-duplicate t
        ;; lsp-ui-doc is redundant with and more invasive than
        ;; `+lookup/documentation'
        lsp-ui-doc-enable nil
        lsp-ui-doc-show-with-mouse nil  ; don't disappear on mouseover
        lsp-ui-doc-position 'at-point
        ;; Don't show symbol definitions in the sideline. They are pretty noisy,
        ;; and there is a bug preventing Flycheck errors from being shown (the
        ;; errors flash briefly and then disappear).
        lsp-ui-sideline-show-hover nil)

  (map! :map lsp-ui-peek-mode-map
        "j"   #'lsp-ui-peek--select-next
        "k"   #'lsp-ui-peek--select-prev
        "C-k" #'lsp-ui-peek--select-prev-file
        "C-j" #'lsp-ui-peek--select-next-file)

  (when (featurep! +peek)
    (set-lookup-handlers! 'lsp-ui-mode :async t
      :definition 'lsp-ui-peek-find-definitions
      :implementations 'lsp-ui-peek-find-implementation
      :references 'lsp-ui-peek-find-references)))


(use-package! helm-lsp
  :when (featurep! :completion helm)
  :commands helm-lsp-workspace-symbol helm-lsp-global-workspace-symbol)


(use-package! lsp-ivy
  :when (featurep! :completion ivy)
  :commands lsp-ivy-workspace-symbol lsp-ivy-global-workspace-symbol)
