;;; org/org-capture/config.el -*- lexical-binding: t; -*-

(add-hook 'org-load-hook #'+org-capture|init t)

;; Sets up two `org-capture' workflows that I like:
;;
;; 1. The traditional way: invoking `org-capture' directly (or through a
;;    command, like :org).
;;
;; 2. Through a org-capture popup frame that is invoked from outside Emacs (the
;;    script is in ~/.emacs.d/bin). This lets me open an org-capture box
;;    anywhere I can call org-capture (whether or not Emacs is open/running),
;;    like, say, from qutebrowser, vimperator, dmenu or a global keybinding.

(defun +org-capture|init ()
  "Set up a sane `org-capture' workflow."
  (setq org-default-notes-file (concat +org-dir "notes.org")
        ;; FIXME This is incomplete!
        org-capture-templates
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
           (file+headline (concat +org-dir "notes.org") "Inbox")
           "* %u %?\n%i" :prepend t)

          ;; ("v" "Vocab" entry
          ;;  (file+headline (concat org-directory "topics/vocab.org") "Unsorted")
          ;;  "** %i%?\n")
          ))

  (when (featurep! :feature evil)
    (add-hook 'org-capture-mode-hook #'evil-insert-state))

  ;; Allows the Emacs mini-frame (opened from an external shell script to run
  ;; and clean up properly) if the frame is named "org-capture".
  (require 'org-capture)
  (require 'org-protocol)
  (defun +org-capture*init (&rest _)
    "Makes sure the org-capture window is the only window in the frame."
    (when (equal "org-capture" (frame-parameter nil 'name))
      (setq mode-line-format nil)
      (delete-other-windows)))
  (advice-add #'org-capture :after #'+org-capture*init)

  (defun +org-capture|finalize ()
    "Closes the frame once org-capture is done."
    (when (equal "org-capture" (frame-parameter nil 'name))
      (when (and (featurep 'persp-mode) persp-mode)
        (+workspace/delete (+workspace-current-name)))
      (delete-frame)))
  (add-hook 'org-capture-after-finalize-hook #'+org-capture|finalize))
