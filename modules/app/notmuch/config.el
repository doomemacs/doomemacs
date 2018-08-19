;;; app/notmuch/config.el -*- lexical-binding: t; -*-

;; FIXME This module is a WIP!

(defvar +notmuch-sync-backend 'gmi
  "Which backend to use. Can be either gmi, mbsync, offlineimap or nil (manual).")

(defvar +notmuch-mail-folder "~/.mail/account.gmail"
  "Where your email folder is located (for use with gmailieer).")

(after! notmuch
  (set-company-backend! 'notmuch-message-mode
    '(notmuch-company (company-ispell :with company-yasnippet)))

  (set-popup-rule! "^\\*notmuch-hello" :side 'left :size 30 :ttl 0)

  (setq notmuch-fcc-dirs nil
        notmuch-show-logo nil
        notmuch-message-headers-visible nil
        message-kill-buffer-on-exit t
        message-send-mail-function 'message-send-mail-with-sendmail
        notmuch-search-oldest-first nil
        send-mail-function 'sendmail-send-it
        ;; sendmail-program "/usr/local/bin/msmtp"
        notmuch-search-result-format
        '(("date" . "%12s ")
          ("count" . "%-7s ")
          ("authors" . "%-30s ")
          ("subject" . "%-72s ")
          ("tags" . "(%s)"))
        notmuch-tag-formats
        '(("unread" (propertize tag 'face 'notmuch-tag-unread)))
        notmuch-hello-sections
        '(notmuch-hello-insert-saved-searches
          notmuch-hello-insert-alltags)
        notmuch-saved-searches
        '((:name "inbox"   :query "tag:inbox not tag:trash" :key "i")
          (:name "flagged" :query "tag:flagged"             :key "f")
          (:name "sent"    :query "tag:sent"                :key "s")
          (:name "drafts"  :query "tag:draft"               :key "d"))
        notmuch-archive-tags '("-inbox" "-unread"))

  ;; (setq-hook! 'notmuch-show-mode-hook line-spacing 0)

  (add-to-list 'doom-real-buffer-functions #'notmuch-interesting-buffer nil #'eq)

  (advice-add #'notmuch-start-notmuch-sentinel :around #'+notmuch*dont-confirm-on-kill-process)

  ;; Visual enhancements
  (defun +notmuch|center-window ()
    (setq-local visual-fill-column-width 90)
    (visual-fill-column-mode))
  (add-hook 'notmuch-show-mode-hook #'+notmuch|center-window)

  ;; modeline doesn't have much use in these modes
  (add-hook! (notmuch-show-mode notmuch-tree-mode notmuch-search-mode)
    #'hide-mode-line-mode))


(def-package! org-mime
  :after (org notmuch)
  :config (setq org-mime-library 'mml))


(def-package! counsel-notmuch
  :when (featurep! :completion ivy)
  :commands counsel-notmuch
  :after notmuch)

(def-package! helm-notmuch
  :when (featurep! :completion helm)
  :commands helm-notmuch
  :after notmuch)

