;;; app/rss/config.el -*- lexical-binding: t; -*-

;; This is an opinionated workflow that turns Emacs into an RSS reader, inspired
;; by apps Reeder and Readkit. It can be invoked via `=rss'. Otherwise, if you
;; don't care for the UI you can invoke elfeed directly with `elfeed'.

(defvar +rss-elfeed-files (list "elfeed.org")
  "Where to look for elfeed.org files, relative to `org-directory'. Can be
absolute paths.")

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
        elfeed-show-entry-switch #'pop-to-buffer
        elfeed-show-entry-delete #'+rss/delete-pane
        shr-max-image-proportion 0.6)

  (set-popup-rule! "^\\*elfeed-entry"
    :size 0.75 :side 'bottom
    :select t :quit nil :ttl t)

  (make-directory elfeed-db-directory t)

  ;; Ensure elfeed buffers are treated as real
  (defun +rss-buffer-p (buf)
    (string-match-p "^\\*elfeed" (buffer-name buf)))
  (add-to-list 'doom-real-buffer-functions #'+rss-buffer-p nil #'eq)

  ;; Enhance readability of a post
  (add-hook 'elfeed-show-mode-hook #'+rss|elfeed-wrap)

  (define-key! (elfeed-search-mode-map elfeed-show-mode-map)
    [remap kill-this-buffer] #'+rss/quit
    [remap kill-buffer]      #'+rss/quit)
  (define-key! elfeed-show-mode-map
    [remap next-buffer]     #'+rss/next
    [remap previous-buffer] #'+rss/previous)
  (when (featurep 'evil)
    (evil-define-key* 'normal elfeed-search-mode-map
      "q"     #'+rss/quit
      "r"     #'elfeed-update
      "s"     #'elfeed-search-live-filter
      (kbd "RET")   #'elfeed-search-show-entry
      (kbd "M-RET") #'elfeed-search-browse-url)
    (evil-define-key* 'normal elfeed-show-mode-map
      "q"  #'elfeed-kill-buffer)
    (evil-define-key* 'motion elfeed-show-mode-map
      "j"  #'evil-next-visual-line
      "k"  #'evil-previous-visual-line)))


(def-package! elfeed-org
  :when (featurep! +org)
  :after elfeed
  :config
  (setq rmh-elfeed-org-files
        (let ((default-directory org-directory))
          (mapcar #'expand-file-name +rss-elfeed-files)))
  (elfeed-org))
