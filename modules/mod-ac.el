(require-packages
 '(auto-complete           ; self-explanity
   auto-complete-config    ; its default config
   fuzzy                   ; fuzzy search engine for auto-complete
   ))

;;;; Auto-completion ;;;;;;;;;;;;;;

(ac-config-default)
(ac-linum-workaround)         ; Fix line number flux bug

(add-hook 'prog-mode-hook 'enable-path-completion)
(setq ac-auto-show-menu nil     ; Suggestions box must be invoked manually (see core-keymaps.el)
      ac-use-menu-map t         ; Enable ac-menu-map map when menu is open
      ac-us-quick-help nil      ; Don't show tooltips unless invoked (see core-keymaps.el)
      ac-fuzzy-cursor-color nil)

(defun enable-path-completion ()
      (add-to-list 'ac-sources 'ac-source-filename)
      (add-to-list 'ac-sources 'ac-source-files-in-current-dir))

;; Tell ido not to care about case
(setq completion-ignore-case t)

;;; Filters ido-matches setting acronynm matches in front of the results
(defadvice ido-set-matches-1 (after ido-smex-acronym-matches activate)
  (if (and (fboundp 'smex-already-running) (smex-already-running)
           (> (length ido-text) 1))
      (let ((regex (concat "^" (mapconcat 'char-to-string ido-text "[^-]*-")))
            (acronym-matches (list))
            (remove-regexes '("-menu-")))
        ;; Creating the list of the results to be set as first
        (dolist (item items)
          (if (string-match (concat regex "[^-]*$") item) ;; strict match
              (add-to-list 'acronym-matches item)
            (if (string-match regex item) ;; appending relaxed match
                (add-to-list 'acronym-matches item t))))

        ;; Filtering ad-return-value
        (dolist (to_remove remove-regexes)
          (setq ad-return-value
                (delete-if (lambda (item)
                             (string-match to_remove item))
                           ad-return-value)))

        ;; Creating resulting list
        (setq ad-return-value
              (append acronym-matches
                      ad-return-value))

        (delete-dups ad-return-value)
        (reverse ad-return-value))))

;;
(provide 'mod-ac)
