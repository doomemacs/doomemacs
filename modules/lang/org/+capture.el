;;; lang/org/+capture.el

;; Sets up two `org-capture' workflows that I like:
;;
;; 1. The traditional way: invoking `org-capture' directly (or through a
;;    command, like :org).
;;
;; 2. Through a org-capture popup frame that is invoked from outside Emacs (the
;;    script is below). This lets me open an org-capture box anywhere I can call
;;    org-capture.sh, like, say, from qutebrowser or vimperator.
;;
;;    #!/usr/bin/env bash
;;    emacsclient -c \
;;        -F "((name . \"org-capture\") (height . 25) (width . 70))" \
;;        --eval "(org-capture nil \"${1:-n}\")"
;;
;;    Place this in, say, org-capture.sh somewhere in your $PATH.

(add-hook '+org-init-hook '+org|init-capture t)

(defun +org|init-capture ()
  "Set up a sane `org-capture' workflow."
  (setq org-default-notes-file (concat +org-dir "notes.org"))

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
           (file+headline (concat +org-dir "notes.org") "Inbox")
           "* %u %?\n%i" :prepend t)

          ;; ("v" "Vocab" entry
          ;;  (file+headline (concat org-directory "topics/vocab.org") "Unsorted")
          ;;  "** %i%?\n")
          ))

  ;; Allows the Emacs mini-frame (opened from an external shell script to run
  ;; and clean up properly) if the frame is named "org-capture".
  (require 'org-capture)
  (require 'org-protocol)
  (defun +org*capture-init (&rest _)
    "Makes sure the org-capture window is the only window in the frame."
    (when (equal "org-capture" (frame-parameter nil 'name))
      (setq mode-line-format nil)
      (delete-other-windows)))
  (advice-add #'org-capture :after #'+org*capture-init)

  (defun +org|capture-finalize ()
    "Closes the frame once org-capture is done."
    (when (equal "org-capture" (frame-parameter nil 'name))
      (when (and (featurep 'persp-mode) persp-mode)
        (+workspace/delete (+workspace-current-name)))
      (delete-frame)))
  (add-hook 'org-capture-after-finalize-hook '+org|capture-finalize))

