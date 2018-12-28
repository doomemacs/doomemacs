;;; tools/dired/config.el -*- lexical-binding: t; -*-

(def-package! dired
  :commands dired-jump
  :init
  (setq ;; Always copy/delete recursively
        dired-recursive-copies  'always
        dired-recursive-deletes 'top
        ;; Auto refresh dired, but be quiet about it
        global-auto-revert-non-file-buffers t
        auto-revert-verbose nil
        dired-hide-details-hide-symlink-targets nil
        ;; files
        image-dired-dir (concat doom-cache-dir "image-dired/")
        image-dired-db-file (concat image-dired-dir "db.el")
        image-dired-gallery-dir (concat image-dired-dir "gallery/")
        image-dired-temp-image-file (concat image-dired-dir "temp-image")
        image-dired-temp-rotate-image-file (concat image-dired-dir "temp-rotate-image"))
  :config
  (setq dired-listing-switches "-aBhl --group-directories-first")

  (when IS-MAC
    ;; Use GNU ls as `gls' from `coreutils' if available. Add `(setq
    ;; dired-use-ls-dired nil)' to your config to suppress the Dired warning
    ;; when not using GNU ls. We must look for `gls' after
    ;; `exec-path-from-shell' was initialized to make sure that `gls' is in
    ;; `exec-path'
    (if-let* ((gls (executable-find "gls")))
        (setq insert-directory-program gls)
      (message "Cannot find `gls`. Install it using `brew install coreutils`")))

  (defun +dired|sort-directories-first ()
    "List directories first in dired buffers."
    (save-excursion
      (let (buffer-read-only)
        (forward-line 2) ;; beyond dir. header
        (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
    (and (featurep 'xemacs)
         (fboundp 'dired-insert-set-properties)
         (dired-insert-set-properties (point-min) (point-max)))
    (set-buffer-modified-p nil))
  (add-hook 'dired-after-readin-hook #'+dired|sort-directories-first)

  ;; Automatically create missing directories when creating new files
  (defun +dired|create-non-existent-directory ()
    (let ((parent-directory (file-name-directory buffer-file-name)))
      (when (and (not (file-exists-p parent-directory))
                 (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
        (make-directory parent-directory t))))
  (add-to-list 'find-file-not-found-functions '+dired|create-non-existent-directory nil #'eq)

  ;; Kill buffer when quitting dired buffers
  (define-key dired-mode-map [remap quit-window] (λ! (quit-window t))))


(def-package! dired-k
  :unless (featurep! +ranger)
  :hook (dired-initial-position . dired-k)
  :hook (dired-after-readin . dired-k-no-revert)
  :config
  (defun +dired*interrupt-process (orig-fn &rest args)
    "Fixes dired-k killing git processes too abruptly, leaving behind disruptive
.git/index.lock files."
    (cl-letf (((symbol-function #'kill-process)
               (symbol-function #'interrupt-process)))
      (apply orig-fn args)))
  (advice-add #'dired-k--start-git-status :around #'+dired*interrupt-process)

  (defun +dired*dired-k-highlight (orig-fn &rest args)
    "Butt out if the requested directory is remote (i.e. through tramp)."
    (unless (file-remote-p default-directory)
      (apply orig-fn args)))
  (advice-add #'dired-k--highlight :around #'+dired*dired-k-highlight))


(def-package! ranger
  :when (featurep! +ranger)
  :after dired
  :init
  ;; set up image-dired to allow picture resize
  (setq image-dired-dir (concat doom-cache-dir "image-dir"))
  :config
  (unless (file-directory-p image-dired-dir)
    (make-directory image-dired-dir))

  (set-popup-rule! "^\\*ranger" :ignore t)

  (setq ranger-override-dired t
        ranger-cleanup-on-disable t
        ranger-omit-regexp "^\.DS_Store$"
        ranger-excluded-extensions '("mkv" "iso" "mp4")
        ranger-deer-show-details nil
        ranger-max-preview-size 10
        dired-omit-verbose nil))


(def-package! all-the-icons-dired
  :when (featurep! +icons)
  :hook (dired-mode . all-the-icons-dired-mode))


(def-package! dired-x
  :hook (dired-mode . dired-omit-mode)
  :config
  (setq dired-omit-verbose nil))


;;
;; Evil integration

(map! :when (featurep! :feature evil +everywhere)
      :after dired
      :map dired-mode-map
      :n "q" #'quit-window
      :m "j" #'dired-next-line
      :m "k" #'dired-previous-line
      :n [mouse-2] #'dired-mouse-find-file-other-window
      :n [follow-link] #'mouse-face
      ;; Commands to mark or flag certain categories of files
      :n "#" #'dired-flag-auto-save-files
      :n "." #'dired-clean-directory
      :n "~" #'dired-flag-backup-files
      ;; Upper case keys (except !) for operating on the marked files
      :n "A" #'dired-do-find-regexp
      :n "C" #'dired-do-copy
      :n "B" #'dired-do-byte-compile
      :n "D" #'dired-do-delete
      :n "gG" #'dired-do-chgrp ;; FIXME: This can probably live on a better binding.
      :n "H" #'dired-do-hardlink
      :n "L" #'dired-do-load
      :n "M" #'dired-do-chmod
      :n "O" #'dired-do-chown
      :n "P" #'dired-do-print
      :n "Q" #'dired-do-find-regexp-and-replace
      :n "R" #'dired-do-rename
      :n "S" #'dired-do-symlink
      :n "T" #'dired-do-touch
      :n "X" #'dired-do-shell-command
      :n "Z" #'dired-do-compress
      :n "c" #'dired-do-compress-to
      :n "!" #'dired-do-shell-command
      :n "&" #'dired-do-async-shell-command
      ;; Comparison commands
      :n "=" #'dired-diff
      ;; Tree Dired commands
      :n "M-C-?" #'dired-unmark-all-files
      :n "M-C-d" #'dired-tree-down
      :n "M-C-u" #'dired-tree-up
      :n "M-C-n" #'dired-next-subdir
      :n "M-C-p" #'dired-prev-subdir
      ;; move to marked files
      :n "M-{" #'dired-prev-marked-file
      :n "M-}" #'dired-next-marked-file
      ;; Make all regexp commands share a `%' prefix:
      ;; We used to get to the submap via a symbol dired-regexp-prefix, but that
      ;; seems to serve little purpose, and copy-keymap does a better job
      ;; without it.
      :n "%" nil
      :n "%u" #'dired-upcase
      :n "%l" #'dired-downcase
      :n "%d" #'dired-flag-files-regexp
      :n "%g" #'dired-mark-files-containing-regexp
      :n "%m" #'dired-mark-files-regexp
      :n "%r" #'dired-do-rename-regexp
      :n "%C" #'dired-do-copy-regexp
      :n "%H" #'dired-do-hardlink-regexp
      :n "%R" #'dired-do-rename-regexp
      :n "%S" #'dired-do-symlink-regexp
      :n "%&" #'dired-flag-garbage-files
      ;; mark
      :n "*" nil
      :n "**" #'dired-mark-executables
      :n "*/" #'dired-mark-directories
      :n "*@" #'dired-mark-symlinks
      :n "*%" #'dired-mark-files-regexp
      :n "*(" #'dired-mark-sexp
      :n "*." #'dired-mark-extension
      :n "*O" #'dired-mark-omitted
      :n "*c" #'dired-change-marks
      :n "*s" #'dired-mark-subdir-files
      :n "*m" #'dired-mark
      :n "*u" #'dired-unmark
      :n "*?" #'dired-unmark-all-files
      :n "*!" #'dired-unmark-all-marks
      :n "U" #'dired-unmark-all-marks
      :n "* <delete>" #'dired-unmark-backward
      :n "* C-n" #'dired-next-marked-file
      :n "* C-p" #'dired-prev-marked-file
      :n "*t" #'dired-toggle-marks
      ;; Lower keys for commands not operating on all the marked files
      :n "a" #'dired-find-alternate-file
      :n "d" #'dired-flag-file-deletion
      :n "gf" #'dired-find-file
      :n "C-m" #'dired-find-file
      :n "gr" #'revert-buffer
      :n "i" #'dired-toggle-read-only
      :n "I" #'dired-maybe-insert-subdir
      :n "J" #'dired-goto-file
      :n "K" #'dired-do-kill-lines
      :n "r" #'dired-do-redisplay
      :n "m" #'dired-mark
      :n "t" #'dired-toggle-marks
      :n "u" #'dired-unmark             ; also "*u"
      :n "W" #'browse-url-of-dired-file
      :n "x" #'dired-do-flagged-delete
      :n "gy" #'dired-show-file-type ;; FIXME: This could probably go on a better key.
      :n "Y" #'dired-copy-filename-as-kill
      :n "+" #'dired-create-directory
      ;; open
      :n "<return>" #'dired-find-file
      :n "S-<return>" #'dired-find-file-other-window
      :n "M-<return>" #'dired-display-file
      :n "gO" #'dired-find-file-other-window
      :n "go" #'dired-view-file
      ;; sort
      :n "o" #'dired-sort-toggle-or-edit
      ;; moving
      :m "gj" #'dired-next-dirline
      :m "gk" #'dired-prev-dirline
      :n "[" #'dired-prev-dirline
      :n "]" #'dired-next-dirline
      :n "<" #'dired-prev-dirline
      :n ">" #'dired-next-dirline
      :n "^" #'dired-up-directory
      :n [?\S-\ ] #'dired-previous-line
      :n [remap next-line] #'dired-next-line
      :n [remap previous-line] #'dired-previous-line
      ;; hiding
      :n "g$" #'dired-hide-subdir ;; FIXME: This can probably live on a better binding.
      :n "M-$" #'dired-hide-all
      :n "(" #'dired-hide-details-mode
      ;; isearch
      :n "M-s a C-s"   #'dired-do-isearch
      :n "M-s a M-C-s" #'dired-do-isearch-regexp
      :n "M-s f C-s"   #'dired-isearch-filenames
      :n "M-s f M-C-s" #'dired-isearch-filenames-regexp
      ;; misc
      :n [remap read-only-mode] #'dired-toggle-read-only
      ;; `toggle-read-only' is an obsolete alias for `read-only-mode'
      :n [remap toggle-read-only] #'dired-toggle-read-only
      :n "g?" #'dired-summary
      :n "<delete>" #'dired-unmark-backward
      :n [remap undo] #'dired-undo
      :n [remap advertised-undo] #'dired-undo
      ;; thumbnail manipulation (image-dired)
      :n "C-t d" #'image-dired-display-thumbs
      :n "C-t t" #'image-dired-tag-files
      :n "C-t r" #'image-dired-delete-tag
      :n "C-t j" #'image-dired-jump-thumbnail-buffer
      :n "C-t i" #'image-dired-dired-display-image
      :n "C-t x" #'image-dired-dired-display-external
      :n "C-t a" #'image-dired-display-thumbs-append
      :n "C-t ." #'image-dired-display-thumb
      :n "C-t c" #'image-dired-dired-comment-files
      :n "C-t f" #'image-dired-mark-tagged-files
      :n "C-t C-t" #'image-dired-dired-toggle-marked-thumbs
      :n "C-t e" #'image-dired-dired-edit-comment-and-tags
      ;; encryption and decryption (epa-dired)
      :n ";d" #'epa-dired-do-decrypt
      :n ";v" #'epa-dired-do-verify
      :n ";s" #'epa-dired-do-sign
      :n ";e" #'epa-dired-do-encrypt)
