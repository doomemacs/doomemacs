;;; core-vars.el --- narf emacs configuration

(defgroup narf nil
  "Narf variables."
  :group 'convenience
  :prefix 'narf-)


;; Buffers/Files ;;;;;;;;;;;;;;;;;;;;;;;

(defconst narf--splash-buffer-name "*narf*")

(defvar narf-auto-minor-mode-alist '()
  "Alist of filename patterns vs corresponding minor mode functions, see
`auto-mode-alist'. All elements of this alist are checked, meaning you can
enable multiple minor modes for the same regexp.")

(defvar narf-unreal-buffers '("^ \\*"
                              "^\\*scratch\\*"
                              "^\\*Backtrace\\*$"
                              "^\\*Warnings\\*$"
                              "^\\*Compile-Log\\*$"
                              "^\\*compilation\\*$"
                              "^\\*Ediff.*\\*$"
                              "^\\*helm.*\\*$"
                              "^\\*eval\\*$"
                              "^\\*Shell Command Output\\*$"
                              "^\\*Async Shell Command\\*$"
                              "^\\*WoMan-Log\\*$"
                              "^\\*Completions\\*$"
                              help-mode
                              image-mode
                              dired-mode
                              reb-mode
                              messages-buffer-mode)
  "A list of regexps or modes whose buffers are considered unreal, and will be
ignored when using `narf:next-real-buffer' and `narf:previous-real-buffer', and
killed by `narf:kill-unreal-buffers'.

`narf:kill-this-buffer' will also gloss over these buffers when finding a new
buffer to display.")

(defvar narf-ignore-buffers '("*Completions*" "*Compile-Log*" "*inferior-lisp*"
                              "*Fuzzy Completions*" "*Apropos*" "*Help*" "*cvs*"
                              "*Buffer List*" "*Ibuffer*" "*esh command on file*"
                              "*helm*" "*WoMan-Log*" "*compilation*")
  "List of buffer names to ignore when using `winner-undo', or `winner-redo'")

(defvar narf-cleanup-processes-alist '(("pry" . ruby-mode)
                                       ("irb" . ruby-mode)
                                       ("ipython" . python-mode))
  "An alist of (process-name . major-mode), that `narf:cleanup-processes' checks
before killing processes. If there are no buffers with matching major-modes, it
gets killed.")

(defvar narf-project-root-files
  '(".git" ".hg" ".svn" ".project" "local.properties" "project.properties"
    "rebar.config" "project.clj" "SConstruct" "pom.xml" "build.sbt"
    "build.gradle" "Gemfile" "requirements.txt" "tox.ini" "package.json"
    "gulpfile.js" "Gruntfile.js" "bower.json" "composer.json" "Cargo.toml"
    "mix.exs")
  "A list of files that count as 'project files', which determine whether a
    folder is the root of a project or not.")


(provide 'core-vars)
;;; core-vars.el ends here
