;;; tools/debugger/config.el -*- lexical-binding: t; -*-

(defvar +debugger--dap-alist
  `(((:lang cc +lsp)         :after ccls        :require (dap-lldb dap-gdb-lldb))
    ((:lang elixir +lsp)     :after elixir-mode :require dap-elixir)
    ((:lang go +lsp)         :after go-mode     :require dap-dlv-go)
    ((:lang gdscript +lsp)   :after gdscript-mode :require dap-gdscript)
    ((:lang java +lsp)       :after java-mode   :require lsp-java)
    ((:lang php +lsp)        :after php-mode    :require dap-php)
    ((:lang python +lsp)     :after python      :require dap-python)
    ((:lang ruby +lsp)       :after ruby-mode   :require dap-ruby)
    ((:lang rust +lsp)       :after rustic      :require (dap-lldb dap-cpptools))
    ((:lang javascript +lsp)
     :after (js2-mode typescript-mode)
     :require (dap-node dap-chrome dap-firefox ,@(if (featurep :system 'windows) '(dap-edge)))))
  "An alist mapping Doom modules to major mode and DAP packages.")


;;
;;; Packages

(use-package! dape
  :defer t
  :preface
  (setq dape-adapter-dir (file-name-concat doom-user-dir "debug-adapters/"))
  :config
  (set-debug-variable! 'dape-debug t)
  (setq dape-buffer-window-arrangement 'right
        dape-inlay-hints t
        dape-cwd-function #'projectile-project-root)

  ;; Mode-line serves no purpose in REPL window.
  (add-hook 'dape-repl-mode-hook #'hide-mode-line-mode)

  ;; Persist breakpoints after closing DAPE.
  (dape-breakpoint-global-mode +1)
  (add-hook 'dape-start-hook #'dape-breakpoint-load 0)
  (add-hook 'dape-stopped-hook #'dape-breakpoint-save 'append)

  ;; HACK: Otherwise, if `dape-default-breakpoints-file's parent directory
  ;;   doesn't exist, `dape-breakpoint-save' will throw an error.
  ;; REVIEW: This should be upstreamed.
  (defadvice! +debugger--make-directory-a (file)
    :before #'dape-breakpoint-save
    (make-directory (file-name-directory (or file dape-default-breakpoints-file)) t)))


;; DEPRECATED
(use-package! dap-mode
  :when (modulep! +lsp)
  :when (modulep! :tools lsp -eglot)
  :hook (dap-mode . dap-tooltip-mode)
  :init
  (setq dap-breakpoints-file (concat doom-data-dir "dap-breakpoints")
        dap-utils-extension-path (concat doom-data-dir "dap-extension/"))
  (after! lsp-mode (require 'dap-mode))
  :config
  (pcase-dolist (`((,category . ,modules) :after ,after :require ,libs)
                 +debugger--dap-alist)
    (when (doom-module-active-p category (car modules) (cadr modules))
      (dolist (lib (ensure-list after))
        (with-eval-after-load lib
          (mapc #'require (ensure-list libs))))))

  (dap-mode 1)

  (define-minor-mode +dap-running-session-mode
    "A mode for adding keybindings to running sessions"
    :init-value nil
    :keymap (make-sparse-keymap)
    (when (bound-and-true-p evil-mode)
      (evil-normalize-keymaps))  ; if you use evil, this is necessary to update the keymaps
    ;; The following code adds to the dap-terminated-hook so that this minor
    ;; mode will be deactivated when the debugger finishes
    (when +dap-running-session-mode
      (let ((session-at-creation (dap--cur-active-session-or-die)))
        (add-hook 'dap-terminated-hook
                  (lambda (session)
                    (when (eq session session-at-creation)
                      (+dap-running-session-mode -1)))))))

  ;; Activate this minor mode when dap is initialized
  (add-hook 'dap-session-created-hook #'+dap-running-session-mode)
  ;; Activate this minor mode when hitting a breakpoint in another file
  (add-hook 'dap-stopped-hook #'+dap-running-session-mode)
  ;; Activate this minor mode when stepping into code in another file
  (add-hook 'dap-stack-frame-changed-hook (lambda (session)
                                            (when (dap--session-running session)
                                              (+dap-running-session-mode 1))))

  (map! :localleader
        :map +dap-running-session-mode-map
        "d" #'dap-hydra))


;; DEPRECATED
(use-package! dap-ui
  :when (modulep! +lsp)
  :when (modulep! :tools lsp -eglot)
  :hook (dap-mode . dap-ui-mode)
  :hook (dap-ui-mode . dap-ui-controls-mode))
