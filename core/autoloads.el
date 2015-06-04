;;; autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "../contrib/goto-last-change" "../contrib/goto-last-change.el"
;;;;;;  (21865 37510 0 0))
;;; Generated autoloads from ../contrib/goto-last-change.el

(autoload 'goto-last-change "../contrib/goto-last-change" "\
Go to the point where the last edit was made in the current buffer.
Repeat the command to go to the second last edit, etc.

To go back to more recent edit, the reverse of this command, use \\[goto-last-change-reverse]
or precede this command with \\[universal-argument] - (minus).

It does not go to the same point twice even if there has been many edits
there. I call the minimal distance between distinguishable edits \"span\".
Set variable `glc-default-span' to control how close is \"the same point\".
Default span is 8.
The span can be changed temporarily with \\[universal-argument] right before \\[goto-last-change]:
\\[universal-argument] <NUMBER> set current span to that number,
\\[universal-argument] (no number) multiplies span by 4, starting with default.
The so set span remains until it is changed again with \\[universal-argument], or the consecutive
repetition of this command is ended by any other command.

When span is zero (i.e. \\[universal-argument] 0) subsequent \\[goto-last-change] visits each and
every point of edit and a message shows what change was made there.
In this case it may go to the same point twice.

This command uses undo information. If undo is disabled, so is this command.
At times, when undo information becomes too large, the oldest information is
discarded. See variable `undo-limit'.

\(fn ARG)" t nil)

(autoload 'goto-last-change-reverse "../contrib/goto-last-change" "\
Go back to more recent changes after \\[goto-last-change] have been used.
See `goto-last-change' for use of prefix argument.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads nil "../contrib/help-fns+" "../contrib/help-fns+.el"
;;;;;;  (21631 21029 0 0))
;;; Generated autoloads from ../contrib/help-fns+.el

(autoload 'describe-command "../contrib/help-fns+" "\
Describe an Emacs command (interactive function).
Equivalent to using a prefix arg with `describe-function'.

If you use Icicles then in Icicle mode keys bound to the commands are
shown next to them in `*Completions*.  You can toggle this keys
display on/off using `C-x C-a'.

\(fn FUNCTION)" t nil)

(autoload 'describe-option "../contrib/help-fns+" "\
Describe an Emacs user variable (option).
Same as using a prefix arg with `describe-variable'.

\(fn VARIABLE &optional BUFFER)" t nil)

(autoload 'describe-option-of-type "../contrib/help-fns+" "\
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

(autoload 'describe-file "../contrib/help-fns+" "\
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

;;;### (autoloads nil "../contrib/hide-mode-line" "../contrib/hide-mode-line.el"
;;;;;;  (21641 7940 0 0))
;;; Generated autoloads from ../contrib/hide-mode-line.el

(autoload 'hide-mode-line "../contrib/hide-mode-line" "\
Toggle the hide-mode-line functionality.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../contrib/hl-todo" "../contrib/hl-todo.el"
;;;;;;  (21835 1957 0 0))
;;; Generated autoloads from ../contrib/hl-todo.el

(autoload 'hl-todo-mode "../contrib/hl-todo" "\
Highlight TODO tags in comments.

\(fn &optional ARG)" t nil)

(defvar global-hl-todo-mode nil "\
Non-nil if Global-Hl-Todo mode is enabled.
See the command `global-hl-todo-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-hl-todo-mode'.")

(custom-autoload 'global-hl-todo-mode "../contrib/hl-todo" nil)

(autoload 'global-hl-todo-mode "../contrib/hl-todo" "\
Toggle Hl-Todo mode in all buffers.
With prefix ARG, enable Global-Hl-Todo mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Hl-Todo mode is enabled in all buffers where
`turn-on-hl-todo-mode-if-desired' would do it.
See `hl-todo-mode' for more information on Hl-Todo mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "../contrib/rotate-text" "../contrib/rotate-text.el"
;;;;;;  (21631 59390 0 0))
;;; Generated autoloads from ../contrib/rotate-text.el

(autoload 'rotate-region "../contrib/rotate-text" "\
Rotate all matches in `rotate-text-rotations' between point and mark.

\(fn BEG END)" t nil)

(autoload 'rotate-word-at-point "../contrib/rotate-text" "\
Rotate word at point based on sets in `rotate-text-rotations'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "defuns-buffers" "defuns-buffers.el" (21869
;;;;;;  35086 0 0))
;;; Generated autoloads from defuns-buffers.el

(autoload 'narf:narrow-to-region-indirect "defuns-buffers" "\
Restrict editing in this buffer to the current region, indirectly.

\(fn START END)" t nil)

(autoload 'narf:widen "defuns-buffers" "\


\(fn)" t nil)

(autoload 'narf:set-region-read-only "defuns-buffers" "\
See http://stackoverflow.com/questions/7410125

\(fn BEGIN END)" nil nil)

(autoload 'narf:set-region-writeable "defuns-buffers" "\
See http://stackoverflow.com/questions/7410125

\(fn BEGIN END)" nil nil)

(autoload 'narf/living-buffer-list "defuns-buffers" "\


\(fn &optional BUFFER-LIST)" nil nil)

(autoload 'narf/add-throwaway-buffer "defuns-buffers" "\


\(fn REGEXP)" nil nil)

(autoload 'narf:cleanup-buffers "defuns-buffers" "\
Kill left-over temporary, dired or buried special buffers

\(fn)" t nil)

(autoload 'narf:cleanup-processes "defuns-buffers" "\


\(fn)" t nil)

(autoload 'narf:kill-matching-buffers "defuns-buffers" "\


\(fn REGEXP &optional BUFFER-LIST)" t nil)

(autoload 'narf:next-real-buffer "defuns-buffers" "\
Switch to the next buffer and avoid special buffers.

\(fn)" t nil)

(autoload 'narf:previous-real-buffer "defuns-buffers" "\
Switch to the previous buffer and avoid special buffers.

\(fn)" t nil)

(autoload 'narf:kill-real-buffer "defuns-buffers" "\
Kill buffer (but only bury scratch buffer)

\(fn)" t nil)
 (autoload 'narf::save-session "defuns-buffers")
 (autoload 'narf::load-session "defuns-buffers")
 (autoload 'narf::new-workgroup "defuns-buffers")
 (autoload 'narf::rename-workgroup "defuns-buffers")
 (autoload 'narf::rename-this-file "defuns-buffers")
 (autoload 'narf::delete-this-file "defuns-buffers")
 (autoload 'narf::create-file "defuns-buffers")
 (autoload 'narf::scratch-buffer "defuns-buffers")
 (autoload 'narf::kill-buried-buffers "defuns-buffers")
 (autoload 'narf::kill-buffers "defuns-buffers")
 (autoload 'narf::cd "defuns-buffers")

;;;***

;;;### (autoloads nil "defuns-code" "defuns-code.el" (21869 24069
;;;;;;  0 0))
;;; Generated autoloads from defuns-code.el

(autoload 'narf/set-build-command "defuns-code" "\


\(fn COMMAND &optional FILE)" nil nil)
 (autoload 'narf::build "defuns-code")
 (autoload 'narf::eval "defuns-code")
 (autoload 'narf::eval-region "defuns-code")
 (autoload 'narf::eval-buffer "defuns-code")
 (autoload 'narf::eval-region-and-replace "defuns-code")

(autoload 'narf/get-interpreter "defuns-code" "\


\(fn)" nil nil)

;;;***

;;;### (autoloads nil "defuns-debug" "defuns-debug.el" (21867 64619
;;;;;;  0 0))
;;; Generated autoloads from defuns-debug.el

(autoload 'what-face "defuns-debug" "\
Tells you the name of the face (point) is on.

\(fn POS)" t nil)

(autoload 'what-col "defuns-debug" "\


\(fn)" t nil)

(autoload 'what-bindings "defuns-debug" "\


\(fn KEY)" nil nil)
 (autoload 'narf::echo "defuns-debug")

;;;***

;;;### (autoloads nil "defuns-edit" "defuns-edit.el" (21866 40579
;;;;;;  0 0))
;;; Generated autoloads from defuns-edit.el

(autoload 'narf:replace-ms-word-chars "defuns-edit" "\
Replace smart quotes and other MS Word verbiage into plain text

\(fn BEG END)" t nil)

(autoload 'narf:replace-email2mailto "defuns-edit" "\
Email address with mailto link

\(fn BEG END)" t nil)

(autoload 'narf:replace-url2anchor "defuns-edit" "\
Link with anchor

\(fn BEG END)" t nil)

(autoload 'narf:goto-line "defuns-edit" "\


\(fn)" t nil)
 (autoload 'narf::align "defuns-edit")
 (autoload 'narf::retab "defuns-edit")
 (autoload 'narf::narrow-indirect-or-widen "defuns-edit")

(autoload 'narf:toggle-delete-trailing-whitespace "defuns-edit" "\


\(fn)" t nil)

;;;***

;;;### (autoloads nil "defuns-extern" "defuns-extern.el" (21866 18425
;;;;;;  0 0))
;;; Generated autoloads from defuns-extern.el

(autoload 'narf/tmux-send "defuns-extern" "\


\(fn COMMAND)" nil nil)
 (autoload 'narf::tmux-run "defuns-extern")
 (autoload 'narf::tmux-chdir "defuns-extern")

;;;***

;;;### (autoloads nil "defuns-mouse" "defuns-mouse.el" (21865 57645
;;;;;;  0 0))
;;; Generated autoloads from defuns-mouse.el

(autoload 'narf/mouse-line-at-click "defuns-mouse" "\
Determine the line number at click

\(fn)" nil nil)

(autoload 'narf/mouse-select-line "defuns-mouse" "\
Set point as *linum-mdown-line*

\(fn EVENT)" t nil)

(autoload 'narf/mouse-select-block "defuns-mouse" "\
Select the current block of text between blank lines.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "defuns-org" "defuns-org.el" (21866 45401 0
;;;;;;  0))
;;; Generated autoloads from defuns-org.el

(autoload 'narf/project-org-filename "defuns-org" "\


\(fn CAT)" t nil)

(autoload 'narf--org-in-list-p "defuns-org" "\


\(fn)" nil nil)

(autoload 'narf/org-insert-item-after "defuns-org" "\
Inserts a new heading or item, depending on the context.

\(fn)" t nil)

(autoload 'narf/org-insert-item-before "defuns-org" "\
Inserts a new heading or item, depending on the context.

\(fn)" t nil)

(autoload 'narf/org-toggle-checkbox "defuns-org" "\


\(fn)" t nil)

(autoload 'narf/org-surround "defuns-org" "\


\(fn DELIM)" nil nil)
 (autoload 'narf::org-insert-image-url "defuns-org")
 (autoload 'narf::org-insert-image "defuns-org")

;;;***

;;;### (autoloads nil "defuns-search" "defuns-search.el" (21866 51196
;;;;;;  0 0))
;;; Generated autoloads from defuns-search.el

(autoload 'narf:ido-find-file "defuns-search" "\


\(fn &optional DIR)" t nil)

(autoload 'narf:ido-find-file-other-window "defuns-search" "\


\(fn &optional DIR)" t nil)

(autoload 'narf:ido-find-project-file "defuns-search" "\


\(fn)" t nil)
 (autoload 'narf::initfiles "defuns-search")
 (autoload 'narf::notes "defuns-search")
 (autoload 'narf::recentf "defuns-search")
 (autoload 'narf::ag-search "defuns-search")
 (autoload 'narf::ag-regex-search "defuns-search")
 (autoload 'narf::ag-regex-cwd "defuns-search")
 (autoload 'narf::ag-regex-search-cwd "defuns-search")
 (autoload 'narf::swoop "defuns-search")
 (autoload 'narf::snippets "defuns-search")

;;;***

;;;### (autoloads nil "defuns-text" "defuns-text.el" (21869 14495
;;;;;;  0 0))
;;; Generated autoloads from defuns-text.el

(autoload 'narf--point-at-bol-non-blank "defuns-text" "\


\(fn)" nil nil)

(autoload 'narf/surrounded-p "defuns-text" "\


\(fn)" nil nil)

(autoload 'narf:backward-kill-to-bol-and-indent "defuns-text" "\
Kill line to the first non-blank character. If invoked again
afterwards, kill line to column 1.

\(fn)" t nil)

(autoload 'narf:move-to-bol "defuns-text" "\
Moves cursor to the first non-blank character on the line. If
already there, move it to the true bol.

\(fn)" t nil)

(autoload 'narf:move-to-eol "defuns-text" "\


\(fn)" t nil)

(autoload 'narf:backward-delete-whitespace-to-column "defuns-text" "\
Delete back to the previous column of whitespace, or as much
whitespace as possible, or just one char if that's not possible.

\(fn)" t nil)

(autoload 'narf:dumb-indent "defuns-text" "\
Inserts a tab character (or spaces x tab-width). Checks if the
auto-complete window is open.

\(fn)" t nil)

(autoload 'narf:inflate-space-maybe "defuns-text" "\
Checks if point is surrounded by {} [] () delimiters and adds a
space on either side of the point if so.

\(fn)" t nil)

(autoload 'narf:deflate-space-maybe "defuns-text" "\
Checks if point is surrounded by {} [] () delimiters, and deletes
spaces on either side of the point if so. Resorts to
`narf:backward-delete-whitespace-to-column' otherwise.

\(fn)" t nil)

(autoload 'narf:newline-and-indent "defuns-text" "\


\(fn)" t nil)

;;;***

;;;### (autoloads nil "defuns-ui" "defuns-ui.el" (21865 64034 0 0))
;;; Generated autoloads from defuns-ui.el

(autoload 'narf:toggle-transparency "defuns-ui" "\


\(fn)" t nil)

(autoload 'narf:toggle-fullscreen "defuns-ui" "\


\(fn)" t nil)

(autoload 'narf:toggle-big-mode "defuns-ui" "\


\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("../contrib/evil-ex-registers.el" "../contrib/flycheck-objc.el"
;;;;;;  "../contrib/ruby-mode-indent-fix.el" "../contrib/shaderlab-mode.el"
;;;;;;  "../contrib/unityjs-mode.el" "../init/autoloads.el" "../init/init-auto-insert.el"
;;;;;;  "../init/init-cc.el" "../init/init-cscope.el" "../init/init-csharp.el"
;;;;;;  "../init/init-eshell.el" "../init/init-fly.el" "../init/init-go.el"
;;;;;;  "../init/init-helm.el" "../init/init-ido.el" "../init/init-java.el"
;;;;;;  "../init/init-js.el" "../init/init-lisp.el" "../init/init-lua.el"
;;;;;;  "../init/init-org.el" "../init/init-php.el" "../init/init-project.el"
;;;;;;  "../init/init-python.el" "../init/init-r.el" "../init/init-regex.el"
;;;;;;  "../init/init-ruby.el" "../init/init-rust.el" "../init/init-scss.el"
;;;;;;  "../init/init-sh.el" "../init/init-swift.el" "../init/init-text.el"
;;;;;;  "../init/init-vc.el" "../init/init-vim.el" "../init/init-web.el"
;;;;;;  "../init/init-workgroups.el" "../init/init-yasnippet.el"
;;;;;;  "../init/narf-bindings.el" "../init/narf-commands.el" "../init/narf-settings.el"
;;;;;;  "benchmark.el" "core-company.el" "core-editor.el" "core-evil.el"
;;;;;;  "core-linux.el" "core-osx.el" "core-splash.el" "core-ui.el"
;;;;;;  "core.el" "defuns.el" "startup.el") (21869 35551 604587 0))

;;;***

(provide 'autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; autoloads.el ends here
