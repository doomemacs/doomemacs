;;; lang/lua/config.el -*- lexical-binding: t; -*-

;; sp's default rules are obnoxious, so disable them
(provide 'smartparens-lua)


;;
;;; Major modes

(use-package! lua-mode
  :defer t
  :init
  ;; lua-indent-level defaults to 3 otherwise. Madness.
  (setq lua-indent-level 2)
  :config
  (set-lookup-handlers! 'lua-mode :documentation 'lua-search-documentation)
  (set-electric! 'lua-mode :words '("else" "end"))
  (set-repl-handler! 'lua-mode #'+lua/open-repl)
  (set-company-backend! 'lua-mode '(company-lua company-yasnippet))

  (when (featurep! +lsp)
    (add-hook 'lua-mode-local-vars-hook #'lsp!)

    (when (featurep! :tools lsp +eglot)
      (defvar +lua-lsp-dir (concat doom-etc-dir "lsp/lua-language-server/")
        "Absolute path to the directory of sumneko's lua-language-server.

This directory MUST contain the 'main.lua' file and be the in-source build of
lua-language-server.")

      (defun +lua-generate-lsp-server-command ()
        ;; The absolute path to lua-language-server binary is necessary because
        ;; the bundled dependencies aren't found otherwise. The only reason this
        ;; is a function is to dynamically change when/if `+lua-lsp-dir' does
        (list (or (executable-find "lua-language-server")
                  (doom-path +lua-lsp-dir
                             (cond (IS-MAC     "bin/macOS")
                                   (IS-LINUX   "bin/Linux")
                                   (IS-WINDOWS "bin/Windows"))
                             "lua-language-server"))
              "-E" "-e" "LANG=en"
              (doom-path +lua-lsp-dir "main.lua")))

      (set-eglot-client! 'lua-mode (+lua-generate-lsp-server-command)))))


(use-package! moonscript
  :when (featurep! +moonscript)
  :defer t
  :config
  (setq-hook! 'moonscript-mode-hook
    moonscript-indent-offset tab-width)
  (add-hook! 'moonscript-mode-hook
             #'+lua-moonscript-fix-single-quotes-h
             #'+lua-moonscript-fontify-interpolation-h)
  (when (featurep! :checkers syntax)
    (require 'flycheck-moonscript nil t)))


(use-package! fennel-mode
  :when (featurep! +fennel)
  :defer t
  :config
  (set-lookup-handlers! 'fennel-mode :definition #'fennel-find-definition)
  (set-repl-handler! 'fennel-mode #'fennel-repl))


;;
;;; Frameworks

(def-project-mode! +lua-love-mode
  :modes '(moonscript-mode lua-mode markdown-mode json-mode)
  :when #'+lua-love-project-root
  :on-load
  (progn
    (set-project-type! 'love2d
      :predicate #'+lua-love-project-root
      :run #'+lua-love-build-command)
    (map! :localleader
          :map +lua-love-mode-map
          "b" #'+lua/run-love-game)))
