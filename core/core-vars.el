;;; core-vars.el --- narf emacs configuration

(defgroup narf nil
  "Narf variables."
  :group 'convenience
  :prefix 'narf-)

(defcustom narf-leader-key "," "The prefix for leader keybindings."
  :group 'narf :type 'string)

(defcustom narf-localleader-key "\\" "The prefix for localleader keybindings."
  :group 'narf)


;; Buffers/Files ;;;;;;;;;;;;;;;;;;;;;;;

(defconst narf--splash-buffer-name "*narf*")

(defvar narf-ignore-buffers '("*Completions*" "*Compile-Log*" "*inferior-lisp*"
                              "*Fuzzy Completions*" "*Apropos*" "*Help*" "*cvs*"
                              "*Buffer List*" "*Ibuffer*" "*esh command on file*")
  "List of buffer names to ignore when using `switch-to-next-buffer',
  `switch-to-previous-buffer', `winner-undo', `winner-redo', or
  `narf:cleanup-buffers'")

(defvar narf-ignore-buffers '("*Completions*" "*Compile-Log*" "*inferior-lisp*"
                                 "*Fuzzy Completions*" "*Apropos*" "*Help*" "*cvs*"
                                 "*Buffer List*" "*Ibuffer*" "*esh command on file*")
  "List of buffer names to ignore when using `switch-to-next-buffer',
  `switch-to-previous-buffer', `winner-undo', `winner-redo', or
  `narf:cleanup-buffers'")

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
