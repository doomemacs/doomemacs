;;; ui/posframe/config.el -*- lexical-binding: t; -*-

(def-package! posframe
  :defer t
  :config
  (add-hook 'doom-cleanup-hook #'posframe-delete-all)
  (add-hook 'doom-escape-hook #'+posframe|delete-on-escape))


(def-package! ivy-posframe
  :when EMACS26+
  :hook (ivy-mode . ivy-posframe-enable)
  :preface
  ;; This function searches the entire `obarray' just to populate
  ;; `ivy-display-functions-props'. There are 15k entries in mine! This is
  ;; wasteful, so...
  (advice-add #'ivy-posframe-setup :override #'ignore)
  :config
  ;; ... let's do it manually
  (dolist (fn (list 'ivy-posframe-display-at-frame-bottom-left
                    'ivy-posframe-display-at-frame-center
                    'ivy-posframe-display-at-point
                    'ivy-posframe-display-at-frame-bottom-window-center
                    'ivy-posframe-display
                    'ivy-posframe-display-at-window-bottom-left
                    'ivy-posframe-display-at-window-center
                    '+posframe-ivy-display-at-frame-center-near-bottom))
    (push (list fn :cleanup 'ivy-posframe-cleanup) ivy-display-functions-props))

  (push '(t . +posframe-ivy-display-at-frame-center-near-bottom) ivy-display-functions-alist)

  ;; posframe doesn't work well with async sources
  (dolist (fn '(swiper counsel-rg counsel-ag counsel-pt counsel-grep counsel-git-grep))
    (push (cons fn nil) ivy-display-functions-alist))

  (setq ivy-height 16
        ivy-fixed-height-minibuffer nil
        ivy-posframe-parameters `((min-width . 90)
                                  (min-height . ,ivy-height)
                                  (internal-border-width . 10))))

;; TODO helm-posframe?
