;;; core-vars.el

(defcustom doom-unreal-buffers
  '("^ ?\\*.+" image-mode dired-mode reb-mode messages-buffer-mode
    tabulated-list-mode comint-mode magit-mode)
  "A list of regexps or modes whose buffers are considered unreal, and will be
ignored when using `doom:next-real-buffer' and `doom:previous-real-buffer' (or
killed by `doom/kill-unreal-buffers', or after `doom/kill-real-buffer')."
  :group 'doom
  :type '(repeat (choice (regexp :tag "Buffer name regexps")
                         (symbol :tag "Major mode"))))

(defcustom doom-ignore-buffers
  '("*Completions*" "*Compile-Log*" "*inferior-lisp*" "*Fuzzy Completions*"
    "*Apropos*" "*Help*" "*cvs*" "*Buffer List*" "*Ibuffer*" "*NeoTree*"
    "*NeoTree*" "*esh command on file*" "*WoMan-Log*" "*compilation*"
    "*use-package*" "*quickrun*" "*eclim: problems*" "*Flycheck errors*"
    "*popwin-dummy*"
    ;; Helm
    "*helm*" "*helm recentf*" "*helm projectile*" "*helm imenu*"
    "*helm company*" "*helm buffers*" "*Helm Css SCSS*" "*helm-ag*"
    "*helm-ag-edit*" "*Helm Swoop*" "*helm M-x*" "*helm mini*"
    "*Helm Completions*" "*Helm Find Files*" "*helm mu*" "*helm mu contacts*"
    "*helm-mode-describe-variable*" "*helm-mode-describe-function*"
    ;; Org
    "*Org todo*" "*Org Links*" "*Agenda Commands*")
  "List of buffer names to ignore when using `winner-undo', or `winner-redo'"
  :group 'doom
  :type '(repeat string))

(defcustom doom-cleanup-processes-alist
  '(("pry" . ruby-mode)
    ("irb" . ruby-mode)
    ("ipython" . python-mode))
  "An alist of (process-name . major-mode), that `doom:cleanup-processes' checks
before killing processes. If there are no buffers with matching major-modes, it
gets killed."
  :group 'doom
  :type '(repeat (cons (string :tag "Process name")
                       (symbol :tag "Associated major mode"))))

(defcustom doom-project-root-files
  '(".git" ".hg" ".svn" ".project" "local.properties" "project.properties"
    "rebar.config" "project.clj" "SConstruct" "pom.xml" "build.sbt"
    "build.gradle" "Gemfile" "requirements.txt" "tox.ini" "package.json"
    "gulpfile.js" "Gruntfile.js" "bower.json" "composer.json" "Cargo.toml"
    "mix.exs")
  "A list of files that count as 'project files', which determine whether a
folder is the root of a project or not."
  :group 'doom
  :type '(repeat string))

(provide 'core-vars)
;;; core-vars.el ends here
