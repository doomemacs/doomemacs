;;; lang/vhdl/config.el -*- lexical-binding: t; -*-

;; treesitter
;; offset
;; formatter
;; lsp-mode and eglot
;;      language servers: TODO VHDL-tool, TODO HDL Checker, VHDL LS, and TODO GHDL LS
;; TODO ligatures: Differentiate between <= (logic) and <= (signal_assignment)


(defgroup doom-vhdl nil
  "VHDL language support for Doom Emacs."
  :group 'languages
  :group 'programming)

(defcustom +vhdl-lsp-server 'vhdl-tool
  "Which LSP server to use for VHDL."
  :group 'doom-vhdl
  :type '(choice (const :tag "VHDL Tool (default)" vhdl-tool)
          (const :tag "HDL Checker" hdl-checker)
          (const :tag "VHDL LS" vhdl-ls)
          (const :tag "GHDL LS" ghdl-ls)))

(defcustom +vhdl-lsp-server-path nil
  "Path to binary server file."
  :group 'doom-vhdl
  :risky t
  :type 'file)


(defun +vhdl-common-config (mode)
  (when (modulep! +lsp)
    (message "Running LSP config")
    (if (modulep! :tools lsp -eglot)

        (with-eval-after-load 'lsp-vhdl
          (setq lsp-vhdl-server      +vhdl-lsp-server
                lsp-vhdl-server-path +vhdl-lsp-server-path)
          (set-lsp-priority! 'lsp-vhdl 0))

      (after! eglot
        (set-eglot-client! mode
                           (lambda (&rest _)
                             (let  ((server-path (or +vhdl-lsp-server-path
                                                     (pcase +vhdl-lsp-server
                                                       ('vhdl-tool "vhdl-tool")
                                                       ('hdl-checker "hdl_checker")
                                                       ('vhdl-ls "vhdl_ls")
                                                       ('ghdl-ls "ghdl-ls")
                                                       (_ "vhdl-tool")))))
                               (list server-path)))))))

  (add-hook (intern (format "%s-local-vars-hook" mode)) #'lsp! 'append))


(use-package! vhdl-mode
  :when (modulep! -tree-sitter)
  :mode ("\\.vhd\\'" "\\.vhdl\\'")
  :hook
  ;; Sync vhdl-mode's tab setting with standard indent-tabs-mode
  (vhdl-mode . (lambda ()
                 (setq-local vhdl-indent-tabs-mode indent-tabs-mode)
                 (setq-local vhdl-basic-offset tab-width)))
  :init
  (set-formatter! 'vhdl-beautify
    (lambda (&rest args)
      (let ((scratch (plist-get args :scratch))
            (callback (plist-get args :callback)))
        (with-current-buffer scratch
          (vhdl-mode)
          (vhdl-beautify-buffer))
        (funcall callback nil)))
    :modes '(vhdl-mode))
  :config
  (+vhdl-common-config 'vhdl-mode))

(use-package! vhdl-ts-mode
  :when (modulep! +tree-sitter)
  :defer t
  :mode ("\\.vhd\\'" "\\.vhdl\\'")
  :hook
  (vhdl-ts-mode . (lambda ()
                    (setq-local vhdl-ts-indent-level tab-width)))
  :init
  (set-tree-sitter! 'vhdl-mode 'vhdl-ts-mode 'vhdl)

  (set-formatter! 'vhdl-ts-beautify
    ;; Get a set of arguments form 'apheleia--run-formatter-function'
    (lambda (&rest args)
      ;; Pick out the need
      (let ((scratch (plist-get args :scratch))
            (callback (plist-get args :callback)))
        ;; Use the given buffer
        (with-current-buffer scratch
          ;; Go into 'vhdl-ts-mode' (starts in 'fundamental-mode')
          (vhdl-ts-mode)
          ;; beautify
          (vhdl-ts-beautify-buffer))
        ;; return with nil errors
        (funcall callback nil)))
    ;; This only works in 'vhdl-ts-mode'
    :modes '(vhdl-ts-mode))
  
  :config
  ;; install tree-sitter grammar unless avaliable
  (unless (treesit-language-available-p 'vhdl)
    (vhdl-ts-install-grammar))

  (+vhdl-common-config 'vhdl-ts-mode))


