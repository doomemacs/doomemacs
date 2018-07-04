;;; ui/doom-modeline/config.el -*- lexical-binding: t; -*-

;;
;; Custom faces
;;

(defgroup +doom-modeline nil
  "TODO"
  :group 'faces)

(defface doom-modeline-buffer-path
  '((t (:inherit (mode-line-emphasis bold))))
  "Face used for the dirname part of the buffer path."
  :group '+doom-modeline)

(defface doom-modeline-buffer-file
  '((t (:inherit (mode-line-buffer-id bold))))
  "Face used for the filename part of the mode-line buffer path."
  :group '+doom-modeline)

(defface doom-modeline-buffer-modified
  '((t (:inherit (error bold) :background nil)))
  "Face used for the 'unsaved' symbol in the mode-line."
  :group '+doom-modeline)

(defface doom-modeline-buffer-major-mode
  '((t (:inherit (mode-line-emphasis bold))))
  "Face used for the major-mode segment in the mode-line."
  :group '+doom-modeline)

(defface doom-modeline-highlight
  '((t (:inherit mode-line-emphasis)))
  "Face for bright segments of the mode-line."
  :group '+doom-modeline)

(defface doom-modeline-panel
  '((t (:inherit mode-line-highlight)))
  "Face for 'X out of Y' segments, such as `+doom-modeline--anzu', `+doom-modeline--evil-substitute' and
`iedit'"
  :group '+doom-modeline)

(defface doom-modeline-info
  `((t (:inherit (success bold))))
  "Face for info-level messages in the modeline. Used by `*vc'."
  :group '+doom-modeline)

(defface doom-modeline-warning
  `((t (:inherit (warning bold))))
  "Face for warnings in the modeline. Used by `*flycheck'"
  :group '+doom-modeline)

(defface doom-modeline-urgent
  `((t (:inherit (error bold))))
  "Face for errors in the modeline. Used by `*flycheck'"
  :group '+doom-modeline)

;; Bar
(defface doom-modeline-bar '((t (:inherit highlight)))
  "The face used for the left-most bar on the mode-line of an active window."
  :group '+doom-modeline)

(defface doom-modeline-eldoc-bar '((t (:inherit shadow)))
  "The face used for the left-most bar on the mode-line when eldoc-eval is
active."
  :group '+doom-modeline)

(defface doom-modeline-inactive-bar '((t (:inherit warning :inverse-video t)))
  "The face used for the left-most bar on the mode-line of an inactive window."
  :group '+doom-modeline)


;;
(if (featurep! +new)
    (load! "+new")
  (load! "+old"))

