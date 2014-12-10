;;; autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "../lib/help-fns+" "../lib/help-fns+.el" (21631
;;;;;;  21029 0 0))
;;; Generated autoloads from ../lib/help-fns+.el

(autoload 'describe-command "../lib/help-fns+" "\
Describe an Emacs command (interactive function).
Equivalent to using a prefix arg with `describe-function'.

If you use Icicles then in Icicle mode keys bound to the commands are
shown next to them in `*Completions*.  You can toggle this keys
display on/off using `C-x C-a'.

\(fn FUNCTION)" t nil)

(autoload 'describe-option "../lib/help-fns+" "\
Describe an Emacs user variable (option).
Same as using a prefix arg with `describe-variable'.

\(fn VARIABLE &optional BUFFER)" t nil)

(autoload 'describe-option-of-type "../lib/help-fns+" "\
Describe an Emacs user OPTION (variable) of a given `defcustom' TYPE.
A prefix argument determines the type-checking behavior:
 - None:         OPTION is defined with TYPE or a subtype of TYPE.
 - Plain `C-u':  OPTION is defined with TYPE or a subtype of TYPE,
                 or its current value is compatible with TYPE.
 - Negative:     OPTION is defined with TYPE (exact match).
 - Non-negative: OPTION is defined with TYPE (exact match),
                 or its current value is compatible with TYPE.

If TYPE is nil (default value) then *all* `defcustom' variables are
potential candidates.  That is different from using `describe-option',
because `describe-option' includes user-variable candidates not
defined with `defcustom' (with `*'-prefixed doc strings).

\(fn TYPE OPTION)" t nil)

(autoload 'describe-file "../lib/help-fns+" "\
Describe the file named FILENAME.
If FILENAME is nil, describe current directory (`default-directory').

Starting with Emacs 22, if the file is an image file then:
 * Show a thumbnail of the image as well.
 * If you have command-line tool `exiftool' installed and in your
   `$PATH' or `exec-path', then show EXIF data (metadata) about the
   image.  See standard Emacs library `image-dired.el' for more
   information about `exiftool'.

If FILENAME is the name of an autofile bookmark and you use library
`Bookmark+', then show also the bookmark information (tags etc.).  In
this case, a prefix arg shows the internal form of the bookmark.

In Lisp code:

Non-nil optional arg INTERNAL-FORM-P shows the internal form.
Non-nil optional arg NO-ERROR-P prints an error message but does not
 raise an error.

\(fn FILENAME &optional INTERNAL-FORM-P NO-ERROR-P)" t nil)

;;;***

;;;### (autoloads nil "../lib/hl-todo" "../lib/hl-todo.el" (21619
;;;;;;  3856 0 0))
;;; Generated autoloads from ../lib/hl-todo.el

(autoload 'hl-todo-mode "../lib/hl-todo" "\
Highlight TODO tags in comments.

\(fn &optional ARG)" t nil)

(defvar global-hl-todo-mode nil "\
Non-nil if Global-Hl-Todo mode is enabled.
See the command `global-hl-todo-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-hl-todo-mode'.")

(custom-autoload 'global-hl-todo-mode "../lib/hl-todo" nil)

(autoload 'global-hl-todo-mode "../lib/hl-todo" "\
Toggle Hl-Todo mode in all buffers.
With prefix ARG, enable Global-Hl-Todo mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Hl-Todo mode is enabled in all buffers where
`turn-on-hl-todo-mode-if-desired' would do it.
See `hl-todo-mode' for more information on Hl-Todo mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "../lib/rotate-text" "../lib/rotate-text.el"
;;;;;;  (21631 59390 0 0))
;;; Generated autoloads from ../lib/rotate-text.el

(autoload 'rotate-region "../lib/rotate-text" "\
Rotate all matches in `rotate-text-rotations' between point and mark.

\(fn BEG END)" t nil)

(autoload 'rotate-word-at-point "../lib/rotate-text" "\
Rotate word at point based on sets in `rotate-text-rotations'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "defuns-buffers" "defuns-buffers.el" (21630
;;;;;;  64935 0 0))
;;; Generated autoloads from defuns-buffers.el

(autoload 'my-narrow-to-region-indirect "defuns-buffers" "\
Restrict editing in this buffer to the current region, indirectly.

\(fn START END)" t nil)

(autoload 'my--cleanup-buffers-add "defuns-buffers" "\


\(fn REGEXP)" nil nil)

(autoload 'my-cleanup-buffers "defuns-buffers" "\
Kill left-over temporary, dired or buried special buffers

\(fn)" t nil)

(autoload 'my-kill-matching-buffers "defuns-buffers" "\


\(fn REGEXP &optional BUFFER-LIST)" t nil)

;;;***

;;;### (autoloads nil "defuns-text" "defuns-text.el" (21630 61093
;;;;;;  0 0))
;;; Generated autoloads from defuns-text.el

(autoload 'my--point-at-bol-non-blank "defuns-text" "\


\(fn)" nil nil)

(autoload 'my--surrounded-p "defuns-text" "\


\(fn)" nil nil)

(autoload 'my\.backward-kill-to-bol-and-indent "defuns-text" "\
Kill line to the first non-blank character. If invoked again
afterwards, kill line to column 1.

\(fn)" t nil)

(autoload 'my\.move-to-bol "defuns-text" "\
Moves cursor to the first non-blank character on the line. If
already there, move it to the true bol.

\(fn)" t nil)

(autoload 'my\.move-to-eol "defuns-text" "\


\(fn)" t nil)

(autoload 'my\.backward-delete-whitespace-to-column "defuns-text" "\
Delete back to the previous column of whitespace, or as much
whitespace as possible, or just one char if that's not possible.

\(fn)" t nil)

(autoload 'my\.dumb-indent "defuns-text" "\
Inserts a tab character (or spaces x tab-width). Checks if the
auto-complete window is open.

\(fn)" t nil)

(autoload 'my\.inflate-space-maybe "defuns-text" "\
Checks if point is surrounded by {} [] () delimiters and adds a
space on either side of the point if so.

\(fn)" t nil)

(autoload 'my\.deflate-space-maybe "defuns-text" "\
Checks if point is surrounded by {} [] () delimiters, and deletes
spaces on either side of the point if so. Resorts to
`my.backward-delete-whitespace-to-column' otherwise.

\(fn)" t nil)

(autoload 'my\.newline-and-indent "defuns-text" "\
Newline and indent; if in a comment, auto-comment and properly
indent the next line.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "defuns-ui" "defuns-ui.el" (21635 60933 0 0))
;;; Generated autoloads from defuns-ui.el

(autoload 'load-dark-theme "defuns-ui" "\


\(fn)" t nil)

(autoload 'load-light-theme "defuns-ui" "\


\(fn)" t nil)

(autoload 'load-font "defuns-ui" "\


\(fn FONT SIZE)" t nil)

(autoload 'toggle-transparency "defuns-ui" "\


\(fn)" t nil)

(autoload 'toggle-theme "defuns-ui" "\


\(fn)" t nil)

(autoload 'toggle-presentation-mode "defuns-ui" "\


\(fn)" t nil)

;;;***

;;;### (autoloads nil "defuns-util" "defuns-util.el" (21631 18940
;;;;;;  0 0))
;;; Generated autoloads from defuns-util.el

(after "s" (defun s-count-lines (s) "Get number of lines in a string" (length (s-lines s))))

(after "f" (defmacro f--exists\? (file dir) `(f-exists\? (expand-file-name ,file ,dir))))

(after "projectile" (defun my--project-root (&optional force-pwd) (if (and (not force-pwd) (projectile-project-p)) (projectile-project-root) default-directory)))

(autoload 'what-face "defuns-util" "\
Tells you the name of the face (point) is on.

\(fn POS)" t nil)

(autoload 'what-col "defuns-util" "\


\(fn)" t nil)

(autoload 'what-bindings "defuns-util" "\


\(fn KEY)" nil nil)

;;;***

;;;### (autoloads nil nil ("../lib/evil-ex-registers.el" "../lib/evil-little-word.el"
;;;;;;  "../lib/ruby-mode-indent-fix.el" "../lib/shaderlab-mode.el"
;;;;;;  "../modules/init-auto-complete.el" "../modules/init-auto-insert.el"
;;;;;;  "../modules/init-company.el" "../modules/init-cpp.el" "../modules/init-cscope.el"
;;;;;;  "../modules/init-csharp.el" "../modules/init-dev.el" "../modules/init-elisp.el"
;;;;;;  "../modules/init-eshell.el" "../modules/init-fly.el" "../modules/init-git.el"
;;;;;;  "../modules/init-go.el" "../modules/init-helm.el" "../modules/init-ido.el"
;;;;;;  "../modules/init-java.el" "../modules/init-js.el" "../modules/init-lua.el"
;;;;;;  "../modules/init-org.el" "../modules/init-php.el" "../modules/init-project.el"
;;;;;;  "../modules/init-projectile.el" "../modules/init-python.el"
;;;;;;  "../modules/init-regex.el" "../modules/init-ruby.el" "../modules/init-scss.el"
;;;;;;  "../modules/init-sh.el" "../modules/init-swift.el" "../modules/init-text.el"
;;;;;;  "../modules/init-tmux.el" "../modules/init-web.el" "../modules/init-yasnippet.el"
;;;;;;  "../my/my-bindings.el" "../my/my-settings.el" "core-editor.el"
;;;;;;  "core-evil.el" "core-linux.el" "core-osx.el" "core-ui.el"
;;;;;;  "core.el" "defuns.el") (21638 3439 736094 0))

;;;***

(provide 'autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; autoloads.el ends here
