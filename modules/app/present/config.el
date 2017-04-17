;;; app/present/config.el

;; Sometimes you just get that urge to show off. I don't have a fancy car, so
;; my code or Emacs will have to do.
;;
;; Sometimes I stream my work on liveedu.tv, or record it on youtube, or peer
;; program over skype/teamviewer, or present at talks -- for all of this, Emacs
;; is my solution.
;;
;;  + `impatient-mode' lets me show off code, as I write it, in real-time over
;;    HTTP (see `+present/buffer')
;;  + `ox-reveal' adds a reveal.js exporter to org-mode
;;  + `org-tree-slide' for presenting org buffers as slides, plus some hacks to
;;    unclutter them.
;;  + and `+present/big-mode' lets me toggle big fonts for streams or
;;    screen-sharing

(defvar +present-big-font (font-spec :family "Fira Mono" :size 18)
  "Font to use when `+present/big-mode' is enabled.")

(defvar +present-scale 7
  "The `text-scale-amount' for `org-tree-slide-mode'.")


;;
;; Plugins
;;

(def-package! impatient-mode
  :commands impatient-mode)


(def-package! centered-window-mode
  :commands centered-window-mode
  :config
  (setq cwm-use-vertical-padding t
        cwm-frame-internal-border 110
        cwm-left-fringe-ratio -10
        cwm-centered-window-width 240))


(when (featurep! :lang org) ;; reveal.js
  (if (bound-and-true-p org-modules-loaded)
      (require 'ox-reveal)
    (add-hook! 'org-load (require 'ox-reveal)))

  (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/"
        org-reveal-mathjax t)

  (def-package! org-tree-slide
    :commands org-tree-slide-mode
    :init
    (after! org
      (map! :map org-mode-map
            "<f8>" #'+present/org-tree-slides
            "<f7>" #'+present/next))
    :config
    (setq org-tree-slide-skip-outline-level 2
          org-tree-slide-activate-message " "
          org-tree-slide-deactivate-message " "
          org-tree-slide-modeline-display nil)
    (org-tree-slide-simple-profile)

    (map! :map org-tree-slide-mode-map
          [right] #'org-tree-slide-move-next-tree
          [left]  #'org-tree-slide-move-previous-tree)

    (add-hook! 'org-tree-slide-mode-after-narrow-hook
      #'(+present|detect-slide +present|add-overlays org-display-inline-images))

    (add-hook! 'org-tree-slide-mode-hook
      (doom/window-zoom)
      (let ((arg (if org-tree-slide-mode +1 -1)))
        (when (featurep 'doom-themes)
          (doom-buffer-mode (* arg -1)))
        (centered-window-mode arg)
        (window-divider-mode (* arg -1))
        (doom-hide-modeline-mode arg)
        (cond (org-tree-slide-mode
               (org-indent-mode -1)
               (text-scale-set +present-scale)
               (ignore-errors (org-preview-latex-fragment '(4)))
               (set-face-attribute 'org-level-2 nil :height 1.4))
              (t
               (org-indent-mode +1)
               (text-scale-set 0)
               (org-remove-latex-fragment-image-overlays)
               (set-face-attribute 'org-level-2 nil :height 1.0)
               (+present|remove-overlays)
               (org-remove-inline-images)))))

    (defun +present*org-tree-slide-narrow-exclude-header (orig-fn &rest args)
      (cl-letf (((symbol-function 'org-narrow-to-subtree)
                 (lambda () (save-excursion
                         (save-match-data
                           (org-with-limited-levels
                            (narrow-to-region
                             (progn (org-back-to-heading t)
                                    (forward-line 1)
                                    (point))
                             (progn (org-end-of-subtree t t)
                                    (when (and (org-at-heading-p) (not (eobp))) (backward-char 1))
                                    (point)))))))))
        (apply orig-fn args)))
    (advice-add #'org-tree-slide--display-tree-with-narrow
                :around #'+present*org-tree-slide-narrow-exclude-header)))

