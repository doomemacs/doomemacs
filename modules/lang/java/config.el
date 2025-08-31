;;; lang/java/config.el -*- lexical-binding: t; -*-

(defvar +java-project-package-roots (list "java/" "test/" "main/" "src/" 1)
  "A list of relative directories (strings) or depths (integer) used by
`+java-current-package' to delimit the namespace from the current buffer's full
file path. Each root is tried in sequence until one is found.

If a directory is encountered in the file path, everything before it (including
it) will be ignored when converting the path into a namespace.

An integer depth is how many directories to pop off the start of the relative
file path (relative to the project root). e.g.

Say the absolute path is ~/some/project/src/java/net/lissner/game/MyClass.java
The project root is ~/some/project
If the depth is 1, the first directory in src/java/net/lissner/game/MyClass.java
  is removed: java.net.lissner.game.
If the depth is 2, the first two directories are removed: net.lissner.game.")

(after! projectile
  (pushnew! projectile-project-root-files "gradlew" "build.gradle"))


;;
;;; java-mode

(when (and (modulep! +lsp)
           (modulep! :tools lsp -eglot))
  (load! "+lsp"))


;;
;;; Common packages

(use-package! java-ts-mode  ; 29.1+ only
  :when (modulep! +tree-sitter)
  :defer t
  :init
  (set-tree-sitter! 'java-mode 'java-ts-mode
    '((java :url "https://github.com/tree-sitter/tree-sitter-java"
            :commit "94703d5a6bed02b98e438d7cad1136c01a60ba2c")
      (doxygen :url "https://github.com/tree-sitter-grammars/tree-sitter-doxygen"
               :commit "1e28054cb5be80d5febac082706225e42eff14e6"))))


(use-package! android-mode
  :commands android-mode
  :init
  (add-hook! '(java-mode-hook groovy-mode-hook nxml-mode-hook)
             #'+java-android-mode-maybe-h)
  :config
  (set-yas-minor-mode! 'android-mode))


(use-package! groovy-mode
  :mode "\\.g\\(?:radle\\|roovy\\)\\'"
  :config
  (set-docsets! 'groovy-mode "Groovy" "Groovy_JDK")
  (set-eval-handler! 'groovy-mode "groovy")
  (set-repl-handler! 'groovy-mode #'+java/open-groovy-repl))
