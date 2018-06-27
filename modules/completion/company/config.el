;;; completion/company/config.el -*- lexical-binding: t; -*-

(def-package! company
  :commands (company-complete-common company-manual-begin company-grab-line)
  :init
  (setq company-idle-delay nil
        company-tooltip-limit 14
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-dabbrev-code-other-buffers t
        company-tooltip-align-annotations t
        company-require-match 'never
        company-global-modes
        '(not comint-mode erc-mode message-mode help-mode gud-mode)
        company-frontends
        '(company-pseudo-tooltip-frontend
          company-echo-metadata-frontend)
        company-backends
        '((:separate company-capf company-yasnippet))
        company-transformers '(company-sort-by-occurrence))
  :config
  (global-company-mode +1))


(def-package! company
  :when (featurep! +auto)
  :defer 2
  :after-call post-self-insert-hook
  :config (setq company-idle-delay 0.2))


(def-package! company-box
  :when (and EMACS26+ (featurep! +childframe))
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-backends-colors nil
        company-box-max-candidates 50
        company-box-icons-yasnippet (all-the-icons-material "short_text" :face 'all-the-icons-green)
        company-box-icons-unknown (all-the-icons-material "find_in_page" :face 'all-the-icons-purple)
        company-box-icons-elisp
        (list (all-the-icons-material "functions" :face 'all-the-icons-red)
              (all-the-icons-material "check_circle" :face 'all-the-icons-blue)
              (all-the-icons-material "stars" :face 'all-the-icons-orange)
              (all-the-icons-material "format_paint" :face 'all-the-icons-pink))
        company-box-icons-lsp
        '((1  . (all-the-icons-material "text_fields"              :face 'all-the-icons-green)) ; text
          (2  . (all-the-icons-material "functions"                :face 'all-the-icons-red))   ; method
          (3  . (all-the-icons-material "functions"                :face 'all-the-icons-red))   ; function
          (4  . (all-the-icons-material "functions"                :face 'all-the-icons-red))   ; constructor
          (5  . (all-the-icons-material "functions"                :face 'all-the-icons-red))   ; field
          (6  . (all-the-icons-material "adjust"                   :face 'all-the-icons-blue))  ; variable
          (7  . (all-the-icons-material "class"                    :face 'all-the-icons-red))   ; class
          (8  . (all-the-icons-material "settings_input_component" :face 'all-the-icons-red))   ; interface
          (9  . (all-the-icons-material "view_module"              :face 'all-the-icons-red))   ; module
          (10 . (all-the-icons-material "settings"                 :face 'all-the-icons-red))   ; property
          (11 . (all-the-icons-material "straighten"               :face 'all-the-icons-red))   ; unit
          (12 . (all-the-icons-material "filter_1"                 :face 'all-the-icons-red))   ; value
          (13 . (all-the-icons-material "plus_one"                 :face 'all-the-icons-red))   ; enum
          (14 . (all-the-icons-material "filter_center_focus"      :face 'all-the-icons-red))   ; keyword
          (15 . (all-the-icons-material "short_text"               :face 'all-the-icons-red))   ; snippet
          (16 . (all-the-icons-material "color_lens"               :face 'all-the-icons-red))   ; color
          (17 . (all-the-icons-material "insert_drive_file"        :face 'all-the-icons-red))   ; file
          (18 . (all-the-icons-material "collections_bookmark"     :face 'all-the-icons-red))   ; reference
          (19 . (all-the-icons-material "folder"                   :face 'all-the-icons-red))   ; folder
          (20 . (all-the-icons-material "people"                   :face 'all-the-icons-red))   ; enumMember
          (21 . (all-the-icons-material "pause_circle_filled"      :face 'all-the-icons-red))   ; constant
          (22 . (all-the-icons-material "streetview"               :face 'all-the-icons-red))   ; struct
          (23 . (all-the-icons-material "event"                    :face 'all-the-icons-red))   ; event
          (24 . (all-the-icons-material "control_point"            :face 'all-the-icons-red))   ; operator
          (25 . (all-the-icons-material "class"                    :face 'all-the-icons-red)))))


(def-package! company-dict
  :defer t
  :config
  (defun +company|enable-project-dicts (mode &rest _)
    "Enable per-project dictionaries."
    (if (symbol-value mode)
        (add-to-list 'company-dict-minor-mode-list mode nil #'eq)
      (setq company-dict-minor-mode-list (delq mode company-dict-minor-mode-list))))
  (add-hook 'doom-project-hook #'+company|enable-project-dicts))

