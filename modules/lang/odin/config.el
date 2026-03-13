;;; lang/odin/config.el -*- lexical-binding: t; -*-

(defun +odin-common-config (mode)
  (when (modulep! +lsp)
    (add-hook (intern (format "%s-local-vars-hook" mode)) #'lsp! 'append))
  (map! :map ,(intern (format "%s-map" mode))
        :localleader
        (:prefix ("b" . "build")
        :desc "odin Build" "b" #'+odin/build
        ;; :desc "odin bundle" "B" #'odin-bundle
        ;; :desc "odin check" "c" #'odin-check
        ;; :desc "odin doc" "d" #'odin-doc
        ;; :desc "odin run" "r" #'odin-run
        ;; :desc "odin strip-semicolon" "s" #'odin-strip-semicolon
        )
        (:prefix ("t" . "test")
        ;; :desc "odin report" "r" #'odin-report
        ;; :desc "odin root" "R" #'odin-root
        :desc "odin test" "t" #'+odin/test
        )))

(use-package! odin-mode
  :mode "\\.odin\\'"
  :config
  (+odin-common-config 'odin-mode))

(use-package! odin-ts-mode
  :when (modulep! +tree-sitter)
  :defer t
  :init
  (set-tree-sitter! 'odin-mode 'odin-ts-mode
    '((odin :url "https://github.com/tree-sitter-grammars/tree-sitter-odin")))
  :config
  (+odin-common-config 'odin-ts-mode))


;;(after! dape
;;  (add-to-list 'dape-configs
;;               `(odin-debug
;;                 modes (odin-mode odin-ts-mode)
;;                 ensure dape-ensure-command
;;                 command "lldb-dap"
;;                 command-cwd dape-cwd-fn
;;                 :type "lldb"
;;                 :request "launch"
;;                 :name "Odin Debug"
;;                 :program "out"
;;                 :args []
;;                 :stopOnEntry nil)))
