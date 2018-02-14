;;; feature/evil/autoload/files.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+evil:delete-this-file "feature/evil/autoload/files" nil t)
(evil-define-command +evil:delete-this-file (&optional filename force-p)
  "Delete FILENAME (defaults to the file associated with current buffer) and
kills the buffer. If FORCE-P, force the deletion (don't ask for confirmation)."
  :repeat nil
  (interactive "<f><!>")
  (doom/delete-this-file (or filename (file-truename buffer-file-name))
                         force-p))

;;;###autoload (autoload '+evil:move-this-file "feature/evil/autoload/files" nil t)
(evil-define-command +evil:move-this-file (new-path &optional force-p)
  "Move current buffer's file to NEW-PATH. Replaces %, # and other vim-esque
filename modifiers (see `+evil*ex-replace-special-filenames'). If FORCE-P,
overwrite the destination file if it exists, without confirmation."
  :repeat nil
  (interactive "<f><!>")
  (when (or (not new-path) (string-empty-p new-path))
    (user-error "No new path was specified"))
  (doom/move-this-file new-path force-p))

;;;###autoload (autoload '+evil:copy-this-file "feature/evil/autoload/files" nil nil)
(evil-define-command +evil:copy-this-file (new-path &optional force-p)
  "Copy current buffer's file to NEW-PATH. Replaces %, # and other vim-esque
filename modifiers (see `+evil*ex-replace-special-filenames'). If FORCE-P,
overwrite the destination file if it exists, without confirmation."
  :repeat nil
  (interactive "<f><!>")
  (when (or (not new-path) (string-empty-p new-path))
    (user-error "No new path was specified"))
  (doom/copy-this-file new-path force-p))

