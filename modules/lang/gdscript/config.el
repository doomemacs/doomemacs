;;; lang/gdscript/config.el -*- lexical-binding: t; -*-

(after! project
  (add-to-list 'project-vc-extra-root-markers "project.godot"))

;; DEPRECATED: Remove when projectile is replaced with project.el
(after! projectile
  (add-to-list 'projectile-project-root-files "project.godot"))


;;
;;; Packages

(use-package! gdscript-mode
  :defer t
  :config
  (set-lookup-handlers! 'gdscript-mode
    :documentation '(gdscript-docs-browse-symbol-at-point :async t))
  (set-formatter! 'gdformat '("gdformat" "-") :modes '(gdscript-mode))
  (set-popup-rule! "^\\*godot " :ttl 0 :quit t)

  (defadvice! +gdscript--dont-focus-output-buffer-a (fn &rest args)
    "Don't move cursor into gdscript compilation window."
    :around #'gdscript-comint--run
    (save-selected-window (apply fn args)))

  ;; eww is clumsy and slow. Best that `gdscript-docs-browse-symbol-at-point'
  ;; simply use the browser.
  ;; REVIEW: Maybe it's permissible if `gdscript-docs-local-path' is set?
  (setq gdscript-docs-use-eww nil)

  ;; Some systems append the version number to the executable, so ensure that
  ;; the path is correct. Note that I avoid setting it to an absolute path to
  ;; preserve TRAMP support.
  (setq gdscript-godot-executable
        (cond ((executable-find gdscript-godot-executable) gdscript-godot-executable)
              ((executable-find "godot") "godot")
              ((executable-find "godot4") "godot4")))

  (when (modulep! +lsp)
    (add-hook 'gdscript-mode-local-vars-hook #'lsp! 'append))

  (after! dtrt-indent
    (add-to-list 'dtrt-indent-hook-mapping-list '(gdscript-mode default gdscript-indent-offset)))

  (map! :localleader
        :map gdscript-mode-map
        (:prefix ("r" . "run")
         :desc "Open project in Godot"     "e" #'gdscript-godot-open-project-in-editor
         :desc "Run project"               "p" #'gdscript-godot-run-project
         :desc "Run debug"                 "d" #'gdscript-godot-run-project-debug
         :desc "Run current scene"         "s" #'gdscript-godot-run-current-scene)
        (:prefix ("d" . "debug")
         :desc "Toggle breakpoint"         "d" #'gdscript-debug-toggle-breakpoint
         :desc "Display breakpoint buffer" "b" #'gdscript-debug-display-breakpoint-buffer
         :desc "Continue execution"        "c" #'gdscript-debug-continue
         :desc "Next"                      "n" #'gdscript-debug-next
         :desc "Step"                      "s" #'gdscript-debug-step)
        (:prefix ("h" . "help")
         :desc "Browse online API"   "b" #'gdscript-docs-browse-api
         :desc "Browse API at point" "f" #'gdscript-docs-browse-symbol-at-point)))
