;;; lang/org/+capture.el --- -*- no-byte-compile: t; -*-

;; Sets up a sane `org-capture' workflow, wherein the org-capture buffer is
;; opened in a popup frame, and can be invoked from outside Emacs as well.
;;
;; See `+org/capture'

(add-hook '+org-init-hook '+org|init-capture t)

(defun +org|init-capture ()
  "Set up a sane `org-capture' workflow."
  (setq org-default-notes-file +org-quicknote-dir)

  (require 'org-capture)
  (require 'org-protocol)
  (@set :popup "*Org Select*" :size 0.4)

  (defadvice org-capture (after make-full-window-frame activate)
    "If org-capture creates a new frame, this initializes it properly, by
deleting other windows and blanking out the mode-line."
    (when (equal "org-capture" (frame-parameter nil 'name))
      (setq mode-line-format nil)
      (delete-other-windows)))

  (defadvice org-capture-finalize (after delete-capture-frame activate)
    "Closes the frame once org-capture is done."
    (when (equal "org-capture" (frame-parameter nil 'name))
      (delete-frame)))

  (setq org-capture-templates
        '(;; TODO: New Task (todo)
          ;; TODO: New vocabulary word

          ("c" "Changelog" entry
           (file+headline (expand-file-name "CHANGELOG.org" (doom/project-root)) "Unreleased")
           "* %?")

          ;; ("p" "Project Notes" entry
          ;;  (file+headline org-default-notes-file "Inbox")
          ;;  "* %u %?\n%i" :prepend t)

          ;; ("m" "Major-mode Notes" entry
          ;;  (file+headline org-default-notes-file "Inbox")
          ;;  "* %u %?\n%i" :prepend t)

          ("n" "Notes" entry
           (file+headline org-default-notes-file "Inbox")
           "* %u %?\n%i" :prepend t)

          ;; ("v" "Vocab" entry
          ;;  (file+headline (concat org-directory "topics/vocab.org") "Unsorted")
          ;;  "** %i%?\n")
          )))

