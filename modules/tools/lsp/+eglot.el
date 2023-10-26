;;; tools/lsp/+eglot.el -*- lexical-binding: t; -*-

(use-package! eglot
  :commands eglot eglot-ensure
  :hook (eglot-managed-mode . +lsp-optimization-mode)
  :init
  (defadvice! +eglot--ensure-available-mode (fn)
    "Run `eglot-ensure' if the current mode has support."
    :around #'eglot-ensure
    (when (alist-get major-mode eglot-server-programs nil nil
                     (lambda (modes key)
                       (if (listp modes)
                           (member key modes)
                         (eq key modes))))
      (funcall fn)))
  (setq eglot-sync-connect 1
        eglot-autoshutdown t
        eglot-send-changes-idle-time 0.5
        ;; NOTE This setting disable the eglot-events-buffer enabling more
        ;;      consistent performance on long running emacs instance.
        ;;      Default is 2000000 lines. After each new event the whole buffer
        ;;      is pretty printed which causes steady performance decrease over time.
        ;;      CPU is spent on pretty priting and Emacs GC is put under high pressure.
        eglot-events-buffer-size 0
        ;; NOTE We disable eglot-auto-display-help-buffer because :select t in
        ;;      its popup rule causes eglot to steal focus too often.
        eglot-auto-display-help-buffer nil)
  (when (and (modulep! :checkers syntax)
             (not (modulep! :checkers syntax +flymake)))
    (setq eglot-stay-out-of '(flymake)))

  :config
  (set-popup-rule! "^\\*eglot-help" :size 0.15 :quit t :select t)
  (set-lookup-handlers! 'eglot--managed-mode
    :definition      #'xref-find-definitions
    :references      #'xref-find-references
    :implementations #'eglot-find-implementation
    :type-definition #'eglot-find-typeDefinition
    :documentation   #'+eglot-lookup-documentation)

  (add-to-list 'doom-debug-variables '(eglot-events-buffer-size . 2000000))

  (defadvice! +lsp--defer-server-shutdown-a (fn &optional server)
    "Defer server shutdown for a few seconds.
This gives the user a chance to open other project files before the server is
auto-killed (which is a potentially expensive process). It also prevents the
server getting expensively restarted when reverting buffers."
    :around #'eglot--managed-mode
    (letf! (defun eglot-shutdown (server)
             (if (or (null +lsp-defer-shutdown)
                     (eq +lsp-defer-shutdown 0))
                 (prog1 (funcall eglot-shutdown server)
                   (+lsp-optimization-mode -1))
               (run-at-time
                (if (numberp +lsp-defer-shutdown) +lsp-defer-shutdown 3)
                nil (lambda (server)
                      (unless (eglot--managed-buffers server)
                        (prog1 (funcall eglot-shutdown server)
                          (+lsp-optimization-mode -1))))
                server)))
      (funcall fn server))))


(use-package! consult-eglot
  :defer t
  :when (modulep! :completion vertico)
  :init
  (map! :map eglot-mode-map [remap xref-find-apropos] #'consult-eglot-symbols))


(use-package! flycheck-eglot
  :when (and (modulep! :checkers syntax)
             (not (modulep! :checkers syntax +flymake)))
  :hook (eglot-managed-mode . flycheck-eglot-mode))
