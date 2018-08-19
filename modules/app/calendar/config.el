;;; app/calendar/config.el -*- lexical-binding: t; -*-

(defvar +calendar-org-gcal-secret-file
  (expand-file-name "private/org/secret.el" doom-modules-dir)
  "TODO")

(defvar +calendar-open-function #'+calendar/open-calendar
  "TODO")


;;
;; Plugins
;;

(def-package! calfw
  :commands (cfw:open-calendar-buffer)
  :config
  ;; better frame for calendar
  (setq cfw:face-item-separator-color nil
        cfw:render-line-breaker 'cfw:render-line-breaker-none
        cfw:fchar-junction ?╋
        cfw:fchar-vertical-line ?┃
        cfw:fchar-horizontal-line ?━
        cfw:fchar-left-junction ?┣
        cfw:fchar-right-junction ?┫
        cfw:fchar-top-junction ?┯
        cfw:fchar-top-left-corner ?┏
        cfw:fchar-top-right-corner ?┓)

  (define-key cfw:calendar-mode-map "q" #'+calendar/quit)

  (when (featurep 'solaire-mode)
    (add-hook 'cfw:calendar-mode-hook #'solaire-mode))
  (add-hook 'cfw:calendar-mode-hook 'hide-mode-line-mode)

  (advice-add #'cfw:render-button :override #'+calendar*cfw:render-button))


(def-package! calfw-org
  :commands (cfw:open-org-calendar
             cfw:org-create-source
             cfw:open-org-calendar-withkevin
             my-open-calendar))


(def-package! org-gcal
  :commands (org-gcal-sync
             org-gcal-fetch
             org-gcal-post-at-point
             org-gcal-delete-at-point)
  :config
  (load-file +calendar-org-gcal-secret-file)
  ;; hack to avoid the deferred.el error
  (defun org-gcal--notify (title mes)
    (message "org-gcal::%s - %s" title mes)))


;; (def-package! alert)
