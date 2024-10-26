;;; doom-modules.el --- module & package management system -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;
;;; Variables

(defvar doom-modules nil
  "A table of enabled modules and metadata. See `doom-modules-initialize'.")

(define-obsolete-variable-alias 'doom-modules-dirs 'doom-module-load-path "3.0.0")
(defvar doom-module-load-path
  (list (file-name-concat doom-user-dir "modules")
        (file-name-concat doom-emacs-dir "modules"))
  "A list of paths where Doom should search for modules.

Order determines priority (from highest to lowest).

Each entry is a string; an absolute path to the root directory of a module tree.
In other words, they should contain a two-level nested directory structure,
where the module's group and name was deduced from the first and second level of
directories. For example: if $DOOMDIR/modules/ is an entry, a
$DOOMDIR/modules/lang/ruby/ directory represents a ':lang ruby' module.")

;;; Module file variables
(defvar doom-module-init-file "init.el"
  "The filename for module early initialization config files.

Init files are loaded early, just after Doom core, and before modules' config
files. They are always loaded, even in non-interactive sessions, and before
`doom-before-modules-init-hook'. Related to `doom-module-config-file'.")

(defvar doom-module-config-file "config.el"
  "The filename for module configuration files.

Config files are loaded later, and almost always in interactive sessions. These
run before `doom-after-modules-config-hook' and after `doom-module-init-file'.")

(defvar doom-module-packages-file "packages.el"
  "The filename for the package configuration file.

Package files are read whenever Doom's package manager wants a manifest of all
desired packages. They are rarely read in interactive sessions (unless the user
uses a straight or package.el command directly).")

(defvar doom-module-metadata-file ".doommodule"
  "The filename for a module's metadata file.

NOT IMPLEMENTED YET. This file contains a module's metadata: their version,
maintainers, checks, features, submodules, debug information, etc. And are used
to locate modules in the user's file tree.")

;; DEPRECATED: Remove in v3, as it will be handled in the CLI
(make-obsolete-variable 'doom-obsolete-modules nil "3.0.0")
(defconst doom-obsolete-modules
  '((:feature (version-control  (:emacs vc) (:ui vc-gutter))
              (spellcheck       (:checkers spell))
              (syntax-checker   (:checkers syntax))
              (evil             (:editor evil))
              (snippets         (:editor snippets))
              (file-templates   (:editor file-templates))
              (workspaces       (:ui workspaces))
              (eval             (:tools eval))
              (lookup           (:tools lookup))
              (debugger         (:tools debugger)))
    (:tools   (rotate-text      (:editor rotate-text))
              (vterm            (:term vterm))
              (password-store   (:tools pass))
              (flycheck         (:checkers syntax))
              (flyspell         (:checkers spell))
              (macos            (:os macos)))
    (:emacs   (electric-indent  (:emacs electric))
              (hideshow         (:editor fold))
              (eshell           (:term eshell))
              (term             (:term term)))
    (:ui      (doom-modeline    (:ui modeline))
              (fci              (:ui fill-column))
              (evil-goggles     (:ui ophints))
              (tabbar           (:ui tabs))
              (pretty-code      (:ui ligatures)))
    (:app     (email            (:email mu4e))
              (notmuch          (:email notmuch)))
    (:lang    (perl             (:lang raku))))
  "A tree alist that maps deprecated modules to their replacement(s).

Each entry is a three-level tree. For example:

  (:feature (version-control (:emacs vc) (:ui vc-gutter))
            (spellcheck (:checkers spell))
            (syntax-checker (:tools flycheck)))

This marks :feature version-control, :feature spellcheck and :feature
syntax-checker modules obsolete. e.g. If :feature version-control is found in
your `doom!' block, a warning is emitted before replacing it with :emacs vc and
:ui vc-gutter.")

(make-obsolete-variable 'doom-inhibit-module-warnings nil "3.0.0")
(defvar doom-inhibit-module-warnings (not noninteractive)
  "If non-nil, don't emit deprecated or missing module warnings at startup.")

;;; Custom hooks
(defcustom doom-before-modules-init-hook nil
  "Hooks run before module init.el files are loaded."
  :group 'doom
  :type 'hook)

(defcustom doom-after-modules-init-hook nil
  "Hooks run after module init.el files are loaded."
  :group 'doom
  :type 'hook)

(defcustom doom-before-modules-config-hook nil
  "Hooks run before module config.el files are loaded."
  :group 'doom
  :type 'hook)

(defcustom doom-after-modules-config-hook nil
  "Hooks run after module config.el files are loaded (but before the user's)."
  :group 'doom
  :type 'hook)


;;
;;; Types

(cl-defstruct doom-module
  "TODO"
  (index 0 :read-only t)
  ;; source
  group
  name
  depth
  flags
  features
  ;; sources
  path
  ;; disabled-p
  ;; frozen-p
  ;; layer-p
  ;; recipe
  ;; alist
  ;; package
  ;; if
  )

(cl-defstruct doom-module-context
  "Hot cache object for the containing Doom module."
  index key path flags features)


;;
;;; `doom-module-context'

(defvar doom-module-context (make-doom-module-context)
  "A `doom-module-context' for the module associated with the current file.

Never set this variable directly, use `with-doom-module'.")

(defmacro with-doom-module (key &rest body)
  "Evaluate BODY with `doom-module-context' informed by KEY."
  (declare (indent 1))
  `(let ((doom-module-context (doom-module-context ,key)))
     (doom-log ":context:module: =%s" doom-module-context)
     ,@body))

(defun doom-module-context (key)
  "Return a `doom-module-context' from KEY.

KEY can be a `doom-module-context', `doom-module', or a `doom-module-key' cons
cell."
  (declare (side-effect-free t))
  (unless key
    (error "Invalid module context: %S" key))
  (or (pcase (type-of key)
        (`doom-module-context key)
        (`doom-module (doom-module->context key))
        (`cons (doom-module (car key) (cdr key))))
      (error "Invalid module context or key: %S" key)))

(defun doom-module<-context (context)
  "Return a `doom-module' plist from CONTEXT."
  (declare (side-effect-free t))
  (doom-module-get (doom-module-context-key context)))

(defun doom-module->context (key)
  "Change a `doom-module' into a `doom-module-context'."
  (pcase-let
      (((doom-module index path flags group name)
        (if (doom-module-p key)
            key (doom-module-get (doom-module-key key)))))
    (make-doom-module-context
     :index index
     :key (cons group name)
     :path path
     :flags flags)))

(defun doom-module (group name &optional property)
  "Return the `doom-module-context' for any active module by GROUP NAME.

Return its PROPERTY, if specified."
  (declare (side-effect-free t))
  (when-let ((context (get group name)))
    (if property
        (aref
         context
         (or (plist-get
              (eval-when-compile
                (cl-loop with i = 1
                         for info in (cdr (cl-struct-slot-info 'doom-module-context))
                         nconc (list (doom-keyword-intern (symbol-name (car info)))
                                     (prog1 i (cl-incf i)))))
              property)
             (error "Unknown doom-module-context property: %s" property)))
      context)))


;;
;;; Module API

(defun doom-modules-initialize (&optional force?)
  "Initializes module metadata."
  (when (or (null doom-modules) force?)
    (setq doom-modules (make-hash-table :test 'equal))
    ;; Register Doom's two virtual module categories, representing Doom's core
    ;; and the user's config; which are always enabled.
    (doom-module--put '(:doom . nil) :path doom-core-dir :depth -110)
    (doom-module--put '(:user . nil) :path doom-user-dir :depth '(-105 . 105))
    ;; DEPRECATED: I intend to phase out our internal usage of `use-package' and
    ;;   move it to a :config use-package module. The macro is far too complex
    ;;   and magical for our needs, but until this move is done, ':config
    ;;   use-package' will remain a hardcoded module for backwards
    ;;   compatibility.
    (doom-module--put '(:config . use-package)
                      :path (doom-module-locate-path '(:config . use-package))
                      :depth -111)
    ;; Load $DOOMDIR/init.el, where the user's `doom!' lives, which will inform
    ;; us of all desired modules.
    (doom-load (file-name-concat doom-user-dir doom-module-init-file)
               'noerror)))

(defun doom-module-key (key)
  "Normalize KEY into a (GROUP . MODULE) tuple representing a Doom module key."
  (declare (pure t) (side-effect-free t))
  (cond ((doom-module-p key)
         (cons (doom-module-group key) (doom-module-name key)))
        ((doom-module-context-p key)
         (doom-module-context-key key))
        ((car-safe key)
         (if (nlistp (cdr-safe key))
             key
           (cons (car key) (cadr key))))
        ((error "Invalid key: %S" key))))

(defun doom-module--has-flag-p (flags wanted-flags)
  "Return t if the list of WANTED-FLAGS satisfies the list of FLAGS."
  (declare (pure t) (side-effect-free error-free))
  (cl-loop with flags = (ensure-list flags)
           for flag in (ensure-list wanted-flags)
           for flagstr = (symbol-name flag)
           if (if (eq ?- (aref flagstr 0))
                  (memq (intern (concat "+" (substring flagstr 1)))
                        flags)
                (not (memq flag flags)))
           return nil
           finally return t))

(defun doom-module--fold-flags (flags)
  "Returns a collapsed list of FLAGS (a list of +/- prefixed symbols).

FLAGS is read in sequence, cancelling out negated flags and removing
duplicates."
  (declare (pure t) (side-effect-free error-free))
  (let (newflags)
    (while flags
      (let* ((flag (car flags))
             (flagstr (symbol-name flag)))
        (when-let ((sym (intern-soft
                         (concat (if (eq ?- (aref flagstr 0)) "+" "-")
                                 (substring flagstr 1)))))
          (setq newflags (delq sym newflags)))
        (cl-pushnew flag newflags :test 'eq))
      (setq flags (cdr flags)))
    (nreverse newflags)))

(defun doom-module-get (key &optional property)
  "Returns the plist for GROUP MODULE. Gets PROPERTY, specifically, if set."
  (declare (side-effect-free t))
  (when-let ((m (gethash key doom-modules)))
    (if property
        (aref
         m (or (plist-get
                (eval-when-compile
                  (cl-loop with i = 1
                           for info in (cdr (cl-struct-slot-info 'doom-module))
                           nconc (list (doom-keyword-intern (symbol-name (car info)))
                                       (prog1 i (cl-incf i)))))
                property)
               (error "Unknown doom-module property: %s" property)))
      m)))

(cl-defun doom-module--put ((group . name) &rest plist)
  "Enable GROUP NAME and associate PLIST with it.

This enables the target module, where GROUP is a keyword, NAME is a symbol, and
PLIST is a property list accepting none, any, or all of the following
properties:

  :group KEYWORD
    Indicating the group this module is in. This doesn't have to match GROUP, as
    it could indicate a module alias.
  :name SYMBOL
    Indicating the name of this module. This doesn't have to match NAME, as it
    could indicate a module alias.
  :path STRING
    Path to the directory where this module lives.
  :depth INT|(INITDEPTH . CONFIGDEPTH)
    Determines module load order. If a cons cell, INITDEPTH determines the load
    order of the module's init.el, while CONFIGDEPTH determines the same for all
    other config files (config.el, packages.el, doctor.el, etc).
  :flags (SYMBOL...)
    A list of activated flags for this module. Will be collapsed into
    pre-existing flags for the module.
  :features (SYMBOL...)
    A list of active features, determined from the module's metadata. Will be
    collapsed into any pre-existing features for the module. NOT IMPLEMENTED
    YET.

\(fn (GROUP . NAME) &key GROUP NAME PATH DEPTH FLAGS FEATURES)"
  (let ((module
         (make-doom-module
          :index (hash-table-count doom-modules)
          :group (or (plist-get plist :group) group)
          :name  (or (plist-get plist :name) name)
          :path  (plist-get plist :path)
          :flags (plist-get plist :flags)
          :features ()  ; TODO
          :depth
          (if (not (plist-member plist :depth))
              '(0 . 0)
            (let ((depth (plist-get plist :depth)))
              (cl-check-type depth (or integer cons))
              (cond ((integerp depth) (cons depth depth))
                    ((consp depth) (cons (or (car depth) 0)
                                         (or (cdr depth) 0)))
                    ((error "Invalid DEPTH value: %S" depth))))))))
    (doom-log 2 "module-put: %s" module)
    (prog1 (puthash (cons group name) module doom-modules)
      ;; PERF: Doom caches module index, flags, and features in symbol plists
      ;;   for fast lookups in `modulep!' and elsewhere. plists are lighter and
      ;;   faster than hash tables for datasets this size, and this information
      ;;   is looked up *very* often.
      (put group name (doom-module->context module)))))

(defun doom-module-active-p (group module &optional flags)
  "Return t if GROUP MODULE is active, and with FLAGS (if given)."
  (declare (side-effect-free t))
  (when-let ((val (doom-module-get (cons group module) (if flags :flags))))
    (or (null flags)
        (doom-module--has-flag-p flags val))))

(defun doom-module-exists-p (group module)
  "Returns t if GROUP MODULE is present in any active source."
  (declare (side-effect-free t))
  (if (doom-module-get group module) t))

(cl-defun doom-module--depth< (keya keyb &optional initorder?)
  "Return t if module with KEY-A comes before another with KEY-B.

If INITORDER? is non-nil, grab the car of the module's :depth, rather than it's
cdr. See `doom-module-put' for details about the :depth property."
  (declare (pure t) (side-effect-free t))
  (let* ((adepth (doom-module-get keya :depth))
         (bdepth (doom-module-get keyb :depth))
         (adepth (if initorder? (car adepth) (cdr adepth)))
         (bdepth (if initorder? (car bdepth) (cdr bdepth))))
    (if (or (null adepth) (null bdepth)
            (= adepth bdepth))
        (< (or (doom-module-get keya :index) 0)
           (or (doom-module-get keyb :index) 0))
      (< adepth bdepth))))

(defun doom-module-list (&optional paths-or-all initorder?)
  "Return a list of (:group . name) module keys in order of their :depth.

PATHS-OR-ALL can either be a non-nil value or a list of directories. If given a
list of directories, return a list of module keys for all modules present
underneath it.  If non-nil, return the same, but search `doom-module-load-path'
(includes :doom and :user). Modules that are enabled are sorted first by their
:depth, followed by disabled modules in lexicographical order (unless a :depth
is specified in their .doommodule).

If INITORDER? is non-nil, sort modules by the CAR of that module's :depth."
  (sort (if paths-or-all
            (delete-dups
             (append (seq-remove #'cdr (doom-module-list nil initorder?))
                     (doom-files-in (if (listp paths-or-all)
                                        paths-or-all
                                      doom-module-load-path)
                                    :map #'doom-module-from-path
                                    :type 'dirs
                                    :mindepth 1
                                    :depth 1)))
          (hash-table-keys doom-modules))
        (doom-rpartial #'doom-module--depth< initorder?)))

(defun doom-module-expand-path (key &optional file)
  "Expands a path to FILE relative to KEY, a cons cell: (GROUP . NAME)

GROUP is a keyword. MODULE is a symbol. FILE is an optional string path.
If the group isn't enabled this returns nil. For finding disabled modules use
`doom-module-locate-path' instead."
  (when-let ((path (doom-module-get key :path)))
    (if file
        (file-name-concat path file)
      path)))

(defun doom-module-locate-path (key &optional file)
  "Searches `doom-module-load-path' to find the path to a module by KEY.

KEY is a cons cell (GROUP . NAME), where GROUP is a keyword (e.g. :lang) and
NAME is a symbol (e.g. \\='python). FILE is a string that will be appended to
the resulting path. If said path doesn't exist, this returns nil, otherwise an
absolute path."
  (let (file-name-handler-alist)
    (if-let ((path (doom-module-expand-path key file)))
        (if (or (null file)
                (file-exists-p path))
            path)
      (cl-destructuring-bind (group . module) (doom-module-key key)
        (let* ((group (doom-keyword-name group))
               (module (if module (symbol-name module)))
               (path (file-name-concat group module file)))
          (if file
              ;; PERF: locate-file-internal is a little faster for finding files,
              ;;   but its interface for finding directories is clumsy.
              (locate-file-internal path doom-module-load-path '("" ".elc" ".el"))
            (cl-loop for default-directory in doom-module-load-path
                     if (file-exists-p path)
                     return (expand-file-name path))))))))

(defun doom-module-locate-paths (module-list file)
  "Return all existing paths to FILE under each module in MODULE-LIST.

MODULE-LIST is a list of cons cells (GROUP . NAME). See `doom-module-list' for
an example."
  (cl-loop for key in (or module-list (doom-module-list))
           if (doom-module-locate-path key file)
           collect it))

(defun doom-module-from-path (path &optional enabled-only?)
  "Returns a cons cell (GROUP . NAME) derived from PATH (a file path).
If ENABLED-ONLY?, return nil if the containing module isn't enabled."
  (let* ((file-name-handler-alist nil)
         (path (expand-file-name path)))
    (save-match-data
      (cond ((string-match "/modules/\\([^/]+\\)/\\([^/]+\\)\\(?:/.*\\)?$" path)
             (when-let* ((group (doom-keyword-intern (match-string 1 path)))
                         (name  (intern (match-string 2 path))))
               (and (or (null enabled-only?)
                        (doom-module-active-p group name))
                    (cons group name))))
            ((string-match (concat "^" (regexp-quote doom-core-dir)) path)
             (cons :doom nil))
            ((string-match (concat "^" (regexp-quote doom-user-dir)) path)
             (cons :user nil))))))

(defun doom-module-load-path (&optional module-load-path)
  "Return a list of file paths to activated modules.

The list is in no particular order and its file paths are absolute. If
MODULE-DIRS is non-nil, include all modules (even disabled ones) available in
those directories."
  (declare (pure t) (side-effect-free t))
  (mapcar #'doom-module-locate-path
          (doom-module-list (or module-load-path doom-module-load-path))))

(defun doom-module-mplist-map (fn mplist)
  "Apply FN to each module in MPLIST."
  (let ((mplist (copy-sequence mplist))
        (inhibit-message doom-inhibit-module-warnings)
        obsolete
        results
        group m)
    (while mplist
      (setq m (pop mplist))
      (cond ((keywordp m)
             (setq group m
                   obsolete (assq m doom-obsolete-modules)))
            ((null group)
             (error "No module group specified for %s" m))
            ((and (listp m) (keywordp (car m)))
             (pcase (car m)
               (:cond
                (cl-loop for (cond . mods) in (cdr m)
                         if (eval cond t)
                         return (prependq! mplist mods)))
               (:if (if (eval (cadr m) t)
                        (push (caddr m) mplist)
                      (prependq! mplist (cdddr m))))
               (test (if (xor (eval (cadr m) t)
                              (eq test :unless))
                         (prependq! mplist (cddr m))))))
            ((catch 'doom-modules
               (let* ((module (if (listp m) (car m) m))
                      (flags  (if (listp m) (cdr m))))
                 (when-let (new (assq module obsolete))
                   (let ((newkeys (cdr new)))
                     (if (null newkeys)
                         (print! (warn "%s module was removed"))
                       (if (cdr newkeys)
                           (print! (warn "%s module was removed and split into the %s modules")
                                   (list group module)
                                   (mapconcat #'prin1-to-string newkeys ", "))
                         (print! (warn "%s module was moved to %s")
                                 (list group module)
                                 (car newkeys)))
                       (push group mplist)
                       (dolist (key newkeys)
                         (push (if flags
                                   (nconc (cdr key) flags)
                                 (cdr key))
                               mplist)
                         (push (car key) mplist))
                       (throw 'doom-modules t))))
                 (doom-log "module: %s %s %s -> %s" group module (or flags "")
                           (doom-module-locate-path (cons group module)))
                 (push (funcall fn (cons group module)
                                :flags (if (listp m) (cdr m))
                                :path (doom-module-locate-path (cons group module)))
                       results))))))
    (when noninteractive
      (setq doom-inhibit-module-warnings t))
    (nreverse results)))


;;
;;; Module config macros

(put :if     'lisp-indent-function 2)
(put :when   'lisp-indent-function 'defun)
(put :unless 'lisp-indent-function 'defun)

(defmacro doom! (&rest modules)
  "Bootstraps DOOM Emacs and its modules.

If the first item in MODULES doesn't satisfy `keywordp', MODULES is evaluated,
otherwise, MODULES is a variadic-property list (a plist whose key may be
followed by one or more values).

This macro does nothing in interactive sessions, but in noninteractive session
iterates through MODULES, enabling and initializing them. The order of modules
in these blocks dictates their load order (unless given an explicit :depth)."
  `(when noninteractive
     (doom-module-mplist-map
      #'doom-module--put
      ,@(if (keywordp (car modules))
            (list (list 'quote modules))
          modules))
     t))

;; DEPRECATED Remove in 3.0
(define-obsolete-function-alias 'featurep! 'modulep! "3.0.0")

(defmacro modulep! (group &optional module &rest flags)
  "Return t if :GROUP MODULE (and +FLAGS) are enabled.

If FLAGS is provided, returns t if GROUP MODULE has all of FLAGS enabled.

  (modulep! :config default +flag)
  (modulep! :config default +flag1 +flag2 +flag3)

GROUP and MODULE may be omitted when this macro is used from a Doom module's
source (except your $DOOMDIR, which is a special module). Like so:

  (modulep! +flag3 +flag1 +flag2)
  (modulep! +flag)

FLAGS can be negated. E.g. This will return non-nil if ':tools lsp' is enabled
without `+eglot':

  (modulep! :tools lsp -eglot)

To interpolate dynamic values, use comma:

  (let ((flag '-eglot))
    (modulep! :tools lsp ,flag))

For more about modules and flags, see `doom!'."
  (if (keywordp group)
      (if flags
          `(doom-module--has-flag-p
            (doom-module (backquote ,group) (backquote ,module) :flags)
            (backquote ,flags))
        `(and (get (backquote ,group) (backquote ,module)) t))
    (let ((flags (delq nil (cons group (cons module flags)))))
      (if (doom-module-context-index doom-module-context)
          `(doom-module--has-flag-p
            ',(doom-module-context-flags doom-module-context)
            (backquote ,flags))
        `(let ((file (file!)))
           (if-let ((module (doom-module-from-path file)))
               (doom-module--has-flag-p
                (doom-module (car module) (cdr module) :flags)
                (backquote ,flags))
             (error "(modulep! %s) couldn't resolve current module from %s"
                    (backquote ,flags) (abbreviate-file-name file))))))))

(provide 'doom-modules)
;;; doom-modules.el ends here
