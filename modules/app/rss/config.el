;;; app/rss/config.el -*- lexical-binding: t; -*-

;; This is an opinionated workflow that turns Emacs into an RSS reader, inspired
;; by apps Reeder and Readkit. It can be invoked via `=rss'. Otherwise, if you
;; don't care for the UI you can invoke elfeed directly with `elfeed'.

(defvar +rss-elfeed-files (list "elfeed.org")
  "Where to look for elfeed.org files, relative to `org-directory'. Can be
absolute paths.")

(defvar +rss-split-direction 'below
  "What direction to pop up the entry buffer in elfeed.")

(defvar +rss-enable-sliced-images t
  "Automatically slice images shown in elfeed-show-mode buffers, making them
easier to scroll through.")


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
    :size 0.75 :actions '(display-buffer-below-selected)
    :select t :quit nil :ttl t)

  (make-directory elfeed-db-directory t)

  ;; Ensure elfeed buffers are treated as real
  (defun +rss-buffer-p (buf)
    (string-match-p "^\\*elfeed" (buffer-name buf)))
  (add-to-list 'doom-real-buffer-functions #'+rss-buffer-p nil #'eq)

  ;; Enhance readability of a post
  (add-hook 'elfeed-show-mode-hook #'+rss|elfeed-wrap)
  (add-hook! 'elfeed-search-mode-hook
    (add-hook 'kill-buffer-hook #'+rss|cleanup nil t))

  ;; Large images are annoying to scroll through, because scrolling follows the
  ;; cursor, so we force shr to insert images in slices.
  (when +rss-enable-sliced-images
    (setq-hook! 'elfeed-show-mode-hook
      shr-put-image-function #'+rss-put-sliced-image
      shr-external-rendering-functions '((img . +rss-render-image-tag-without-underline))))

  ;; Keybindings
  (after! elfeed-show
    (define-key! elfeed-show-mode-map
      [remap next-buffer]     #'+rss/next
      [remap previous-buffer] #'+rss/previous))
  (when (featurep! :feature evil +everywhere)
    (evil-define-key 'normal elfeed-search-mode-map
      "q" #'elfeed-kill-buffer
      "r" #'elfeed-search-update--force
      (kbd "M-RET") #'elfeed-search-browse-url)))


(def-package! elfeed-org
  :when (featurep! +org)
  :after elfeed
  :config
  (setq rmh-elfeed-org-files
        (let ((default-directory org-directory))
          (mapcar #'expand-file-name +rss-elfeed-files)))
  (elfeed-org))
