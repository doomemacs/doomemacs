;;; app/calendar/config.el -*- lexical-binding: t; -*-

(defvar +calendar-open-function #'+calendar/open-calendar
  "TODO")


;;
;; Packages

(use-package! calfw
  :commands cfw:open-calendar-buffer
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

  (set-popup-rule! "^\\*cfw:details" :quit t :ttl 0 :select t :size 0.4)

  (define-key cfw:calendar-mode-map "q" #'+calendar/quit)
  (when (modulep! :editor evil +everywhere)
    (set-evil-initial-state! '(cfw:calendar-mode cfw:details-mode) 'motion)
    (add-hook! (cfw:calendar-mode cfw:details-mode) #'evil-normalize-keymaps)
    (map! (:map cfw:calendar-mode-map
           :m "q"   #'+calendar/quit
           :m "SPC" #'cfw:show-details-command
           :m "RET" #'cfw:show-details-command
           :m "TAB"     #'cfw:navi-prev-item-command
           :m [tab]     #'cfw:navi-prev-item-command
           :m [backtab] #'cfw:navi-next-item-command
           :m "$"   #'cfw:navi-goto-week-end-command
           :m "."   #'cfw:navi-goto-today-command
           :m "<"   #'cfw:navi-previous-month-command
           :m ">"   #'cfw:navi-next-month-command
           :m "C-h" #'cfw:navi-previous-month-command
           :m "C-l" #'cfw:navi-next-month-command
           :m "D"   #'cfw:change-view-day
           :m "M"   #'cfw:change-view-month
           :m "T"   #'cfw:change-view-two-weeks
           :m "W"   #'cfw:change-view-week
           :m "^"   #'cfw:navi-goto-week-begin-command
           :m "gr"  #'cfw:refresh-calendar-buffer
           :m "h"   #'cfw:navi-previous-day-command
           :m "H"   #'cfw:navi-goto-first-date-command
           :m "j"   #'cfw:navi-next-week-command
           :m "k"   #'cfw:navi-previous-week-command
           :m "l"   #'cfw:navi-next-day-command
           :m "L"   #'cfw:navi-goto-last-date-command
           :m "t"   #'cfw:navi-goto-today-command)
          (:map cfw:details-mode-map
           :m "SPC" #'cfw:details-kill-buffer-command
           :m "RET" #'cfw:details-kill-buffer-command
           :m "TAB"     #'cfw:details-navi-prev-item-command
           :m [tab]     #'cfw:details-navi-prev-item-command
           :m [backtab] #'cfw:details-navi-next-item-command
           :m "q"   #'cfw:details-kill-buffer-command
           :m "C-h" #'cfw:details-navi-prev-command
           :m "C-l" #'cfw:details-navi-next-command
           :m "C-k" #'cfw:details-navi-prev-item-command
           :m "C-j" #'cfw:details-navi-next-item-command)))

  (add-hook 'cfw:calendar-mode-hook #'doom-mark-buffer-as-real-h)
  (add-hook 'cfw:calendar-mode-hook #'hide-mode-line-mode)

  (advice-add #'cfw:render-button :override #'+calendar-cfw:render-button-a))


(use-package! calfw-org
  :commands (cfw:open-org-calendar
             cfw:org-create-source
             cfw:org-create-file-source
             cfw:open-org-calendar-withkevin))


(use-package! calfw-cal
  :commands (cfw:cal-create-source))


(use-package! calfw-ical
  :commands (cfw:ical-create-source))


(use-package! org-gcal
  :defer t
  :init
  (defvar org-gcal-dir (concat doom-cache-dir "org-gcal/"))
  (defvar org-gcal-token-file (concat org-gcal-dir "token.gpg")))
