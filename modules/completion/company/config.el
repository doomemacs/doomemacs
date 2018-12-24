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
        '(not erc-mode message-mode help-mode gud-mode eshell-mode)
        company-backends '(company-capf)
        company-frontends
        '(company-pseudo-tooltip-frontend
          company-echo-metadata-frontend))
  :config
  (add-hook 'company-mode-hook #'+company|init-backends)
  (global-company-mode +1))


(def-package! company
  :when (featurep! +auto)
  :defer 2
  :after-call post-self-insert-hook
  :config (setq company-idle-delay 0.1))


(def-package! company-tng
  :when (featurep! +tng)
  :defer 2
  :after-call post-self-insert-hook
  :config
  (add-to-list 'company-frontends 'company-tng-frontend)
  (define-key! company-active-map
    "RET"       nil
    [return]    nil
    "TAB"       #'company-select-next
    [tab]       #'company-select-next
    "<backtab>" #'company-select-previous
    [backtab]   #'company-select-previous))


;;
;; Packages

(def-package! company-prescient
  :hook (company-mode . company-prescient-mode)
  :config
  (setq prescient-save-file (concat doom-cache-dir "prescient-save.el"))
  (prescient-persist-mode +1))


(def-package! company-box
  :when (and EMACS26+ (featurep! +childframe))
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-backends-colors nil
        company-box-max-candidates 50
        company-box-icons-yasnippet (all-the-icons-material "short_text" :height 0.8 :face 'all-the-icons-green)
        company-box-icons-unknown (all-the-icons-material "find_in_page" :height 0.8 :face 'all-the-icons-purple)
        company-box-icons-elisp
        (list (all-the-icons-material "functions"                  :height 0.8 :face 'all-the-icons-red)
              (all-the-icons-material "check_circle"               :height 0.8 :face 'all-the-icons-blue)
              (all-the-icons-material "stars"                      :height 0.8 :face 'all-the-icons-orange)
              (all-the-icons-material "format_paint"               :height 0.8 :face 'all-the-icons-pink))
        company-box-icons-lsp
        `((1  . ,(all-the-icons-material "text_fields"              :height 0.8 :face 'all-the-icons-green)) ; text
          (2  . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))   ; method
          (3  . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))   ; function
          (4  . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))   ; constructor
          (5  . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))   ; field
          (6  . ,(all-the-icons-material "adjust"                   :height 0.8 :face 'all-the-icons-blue))  ; variable
          (7  . ,(all-the-icons-material "class"                    :height 0.8 :face 'all-the-icons-red))   ; class
          (8  . ,(all-the-icons-material "settings_input_component" :height 0.8 :face 'all-the-icons-red))   ; interface
          (9  . ,(all-the-icons-material "view_module"              :height 0.8 :face 'all-the-icons-red))   ; module
          (10 . ,(all-the-icons-material "settings"                 :height 0.8 :face 'all-the-icons-red))   ; property
          (11 . ,(all-the-icons-material "straighten"               :height 0.8 :face 'all-the-icons-red))   ; unit
          (12 . ,(all-the-icons-material "filter_1"                 :height 0.8 :face 'all-the-icons-red))   ; value
          (13 . ,(all-the-icons-material "plus_one"                 :height 0.8 :face 'all-the-icons-red))   ; enum
          (14 . ,(all-the-icons-material "filter_center_focus"      :height 0.8 :face 'all-the-icons-red))   ; keyword
          (15 . ,(all-the-icons-material "short_text"               :height 0.8 :face 'all-the-icons-red))   ; snippet
          (16 . ,(all-the-icons-material "color_lens"               :height 0.8 :face 'all-the-icons-red))   ; color
          (17 . ,(all-the-icons-material "insert_drive_file"        :height 0.8 :face 'all-the-icons-red))   ; file
          (18 . ,(all-the-icons-material "collections_bookmark"     :height 0.8 :face 'all-the-icons-red))   ; reference
          (19 . ,(all-the-icons-material "folder"                   :height 0.8 :face 'all-the-icons-red))   ; folder
          (20 . ,(all-the-icons-material "people"                   :height 0.8 :face 'all-the-icons-red))   ; enumMember
          (21 . ,(all-the-icons-material "pause_circle_filled"      :height 0.8 :face 'all-the-icons-red))   ; constant
          (22 . ,(all-the-icons-material "streetview"               :height 0.8 :face 'all-the-icons-red))   ; struct
          (23 . ,(all-the-icons-material "event"                    :height 0.8 :face 'all-the-icons-red))   ; event
          (24 . ,(all-the-icons-material "control_point"            :height 0.8 :face 'all-the-icons-red))   ; operator
          (25 . ,(all-the-icons-material "class"                    :height 0.8 :face 'all-the-icons-red))))

  ;; Until sebastiencs/company-box#40 is merged
  (defun +company*box-frontend-even-if-single (command)
    (cond ((or (eq command 'hide)
               (equal company-candidates-length 0))
           (company-box-hide))
          ((eq command 'update)
           (company-box-show))
          ((eq command 'post-command)
           (company-box--post-command))))
  (advice-add #'company-box-frontend :override #'+company*box-frontend-even-if-single))


(def-package! company-dict
  :defer t
  :config
  (defun +company|enable-project-dicts (mode &rest _)
    "Enable per-project dictionaries."
    (if (symbol-value mode)
        (add-to-list 'company-dict-minor-mode-list mode nil #'eq)
      (setq company-dict-minor-mode-list (delq mode company-dict-minor-mode-list))))
  (add-hook 'doom-project-hook #'+company|enable-project-dicts))

