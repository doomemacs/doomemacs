;;; app/rss/config.el -*- lexical-binding: t; -*-

;; This is an opinionated workflow that turns Emacs into an RSS reader, inspired
;; by apps Reeder and Readkit. It can be invoked via `=rss'. Otherwise, if you
;; don't care for the UI you can invoke elfeed directly with `elfeed'.

(defvar +rss-org-dir (concat +org-dir "/rss/")
  "Where RSS org files are located.")

(defvar +rss-elfeed-files (list "elfeed.org")
  "The files that configure `elfeed's rss feeds.")

(defvar +rss-split-direction 'below
  "What direction to pop up the entry buffer in elfeed.")


;;
;; Packages
;;

(def-package! elfeed
  :commands elfeed
  :config
  (setq elfeed-search-filter "@2-week-ago "
        elfeed-db-directory (concat doom-local-dir "elfeed/db/")
        elfeed-enclosure-default-dir (concat doom-local-dir "elfeed/enclosures/")
        elfeed-show-entry-switch #'+rss-popup-pane
        elfeed-show-entry-delete #'+rss/delete-pane
        shr-max-image-proportion 0.6)

  (make-directory elfeed-db-directory t)

  ;; Ensure elfeed buffers are treated as real
  (cl-pushnew (lambda (buf) (string-match-p "^\\*elfeed" (buffer-name buf)))
              doom-real-buffer-functions)

  ;; Enhance readability of a post
  (add-hook 'elfeed-show-mode-hook #'+rss|elfeed-wrap)

  (map! (:map (elfeed-search-mode-map elfeed-show-mode-map)
          [remap doom/kill-this-buffer] "q"
          [remap kill-this-buffer]      "q"
          [remap kill-buffer]           "q")

        (:map elfeed-search-mode-map
          :n "q"   #'+rss/quit
          :n "r"   #'elfeed-update
          :n "s"   #'elfeed-search-live-filter
          :n "RET" #'elfeed-search-show-entry)

        (:map elfeed-show-mode-map
          :n "q"  #'elfeed-kill-buffer
          :m "j"  #'evil-next-visual-line
          :m "k"  #'evil-previous-visual-line
          [remap doom/next-buffer]      #'+rss/next
          [remap doom/previous-buffer]  #'+rss/previous
          [remap next-buffer]           #'+rss/next
          [remap previous-buffer]       #'+rss/previous)))


(def-package! elfeed-org
  :after elfeed
  :config
  (setq rmh-elfeed-org-files
        (let ((default-directory +rss-org-dir))
          (mapcar #'expand-file-name +rss-elfeed-files)))
  (elfeed-org))
