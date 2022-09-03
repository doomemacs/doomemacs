;;; checkers/grammar/config.el -*- lexical-binding: t; -*-

(use-package! langtool
  :unless (modulep! +lsp)
  :commands (langtool-check
             langtool-check-done
             langtool-show-message-at-point
             langtool-correct-buffer)
  :init (setq langtool-default-language "en-US")
  :config
  (unless (or langtool-bin
              langtool-language-tool-jar
              langtool-java-classpath)
    (cond (IS-MAC
           (cond
            ;; is user using home brew?
            ((file-directory-p "/usr/local/Cellar/languagetool")
             (setq langtool-language-tool-jar
                   (locate-file "libexec/languagetool-commandline.jar"
                                (doom-files-in "/usr/local/Cellar/languagetool"
                                               :type 'dirs
                                               :depth 2))))
            ;; macports compatibility
            ((file-directory-p "/opt/local/share/java/LanguageTool")
             (setq langtool-java-classpath "/opt/local/share/java/LanguageTool/*"))))
          (IS-LINUX
           (setq langtool-java-classpath "/usr/share/languagetool:/usr/share/java/languagetool/*")))))


(use-package! lsp-ltex
  :when (modulep! +lsp)
  :unless (modulep! :tools lsp +eglot)
  :commands (+lsp-ltex-toggle
             +lsp-ltex-enable
             +lsp-ltex-disable
             +lsp-ltex-setup)
  :hook ((text-mode latex-mode LaTeX-mode org-mode markdown-mode) . #'+lsp-ltex-setup)
  :config
  ;; Disable by default, can be enabled in a ber buffer basis,
  ;; Currentrly, launching LSP in some buffers (like org-msg-mode) can cause
  ;; annoying messages, as LSP tries to guess the project root.
  (add-to-list 'lsp-disabled-clients 'ltex-ls)
  :init
  (setq lsp-ltex-check-frequency "save" ;; Less overhead than the default "edit"
        lsp-ltex-log-level "warning" ;; No need to log everything
        ;; Path in which, interactively added words and rules will be stored.
        lsp-ltex-user-rules-path (expand-file-name "lsp-ltex" doom-data-dir))

  ;; When n-gram data sets are available, use them to detect errors with words
  ;; that are often confused (like their and there).
  (when (file-directory-p "/usr/share/ngrams")
    (setq lsp-ltex-additional-rules-language-model "/usr/share/ngrams"))

  (defun +lsp-ltex-setup ()
    "Load LTeX LSP server."
    (interactive)
    (require 'lsp-ltex)
    (when (+lsp-ltex--enabled-p)
      (lsp-deferred)))

  (defun +lsp-ltex--enabled-p ()
    (not (memq 'ltex-ls lsp-disabled-clients)))

  (defun +lsp-ltex-enable ()
    "Enable LTeX LSP for the current buffer."
    (interactive)
    (unless (+lsp-ltex--enabled-p)
      (setq-local lsp-disabled-clients (delq 'ltex-ls lsp-disabled-clients))
      (message "Enabled ltex-ls"))
    (+lsp-ltex-setup))

  (defun +lsp-ltex-disable ()
    "Disable LTeX LSP for the current buffer."
    (interactive)
    (when (+lsp-ltex--enabled-p)
      (setq-local lsp-disabled-clients (cons 'ltex-ls lsp-disabled-clients))
      (lsp-disconnect)
      (message "Disabled ltex-ls")))

  (defun +lsp-ltex-toggle ()
    "Toggle LTeX LSP for the current buffer."
    (interactive)
    (if (+lsp-ltex--enabled-p)
        (+lsp-ltex-disable)
      (+lsp-ltex-enable)))

  (map! :localleader
        :map (text-mode-map latex-mode-map LaTeX-mode-map org-mode-map markdown-mode-map)
        :desc "Toggle grammar check" "G" #'+lsp-ltex-toggle))


;; Detects weasel words, passive voice and duplicates. Proselint would be a
;; better choice.
(use-package! writegood-mode
  :hook (org-mode markdown-mode rst-mode asciidoc-mode latex-mode LaTeX-mode)
  :config
  (map! :localleader
        :map writegood-mode-map
        "g" #'writegood-grade-level
        "r" #'writegood-reading-ease))
