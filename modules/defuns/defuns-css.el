;;; defuns-scss.el

;;;###autoload
(defun doom/css-toggle-inline-or-block ()
  "Toggles between a SCSS multiline block and one-line block."
  (interactive)
  (save-excursion
    (let* ((bounds (ignore-errors (evil-a-curly)))
           beg end)
      (unless bounds
        (user-error "No block found"))
      (setq beg (car bounds)
            end (cadr bounds))
      (if (= (line-number-at-pos beg) (line-number-at-pos end))
          (save-excursion
            (goto-char (1+ beg)) (insert "\n")
            (unless (string-match ";[\s\t]*}$" (buffer-substring-no-properties beg (1+ end)))
              (goto-char end) (insert "\n"))
            (replace-regexp ";[\s\t]*" ";\n" nil beg (1+ end))
            (setq end (cadr (evil-a-curly)))
            (evil-indent beg end)
            (delete-trailing-whitespace beg end))
        (goto-char beg)
        (evil-join beg end)
        (goto-char (1+ beg))
        (just-one-space)
        (goto-char (cadr (evil-inner-curly)))
        (just-one-space)))))

;;;###autoload
(defalias 'doom/sass-build 'doom/scss-build)

;;;###autoload
(defun doom/scss-build ()
  "Compile all sass/scss files in project"
  (interactive)
  (if (bound-and-true-p gulpjs-project-mode)
      (let ((default-directory (doom/project-root)))
        (compile "gulp sass"))
    (let ((scss-dir (f-slash (or (f-traverse-upwards (lambda (d)
                                                       (string-match-p "/\\(\\(s[ca]\\|c\\)ss\\|styles\\)/?$" d))
                                                     default-directory)
                                 default-directory))))
      (compile (format "%s %s --update '%s':'%s'"
                       scss-sass-command
                       (mapconcat 'identity scss-sass-options " ")
                       scss-dir
                       (or scss-output-directory
                           (awhen (f-traverse-upwards (lambda (d)
                                                        (f-dir? (format "%s/css" d)))
                                                      default-directory)
                             (format "%s/css" it))
                           "."))))))

(defface doom/counsel-css-selector-depth-face-1
  '((((class color) (background dark)) (:foreground "#ffff00"))
    (((class color) (background light)) (:foreground "#0000ff"))
    (t (:foreground "#ffff00")))
  "Selector depth 1")
(defface doom/counsel-css-selector-depth-face-2
  '((((class color) (background dark)) (:foreground "#ffdd00"))
    (((class color) (background light)) (:foreground "#3300ff"))
    (t (:foreground "#ffdd00")))
  "Selector depth 2")
(defface doom/counsel-css-selector-depth-face-3
  '((((class color) (background dark)) (:foreground "#ffbb00"))
    (((class color) (background light)) (:foreground "#6600ff"))
    (t (:foreground "#ffbb00")))
  "Selector depth 3")
(defface doom/counsel-css-selector-depth-face-4
  '((((class color) (background dark)) (:foreground "#ff9900"))
    (((class color) (background light)) (:foreground "#9900ff"))
    (t (:foreground "#ff9900")))
  "Selector depth 4")
(defface doom/counsel-css-selector-depth-face-5
  '((((class color) (background dark)) (:foreground "#ff7700"))
    (((class color) (background light)) (:foreground "#cc00ff"))
    (t (:foreground "#ff7700")))
  "Selector depth 5")
(defface doom/counsel-css-selector-depth-face-6
  '((((class color) (background dark)) (:foreground "#ff5500"))
    (((class color) (background light)) (:foreground "#ff00ff"))
    (t (:foreground "#ff5500")))
  "Selector depth 6")

(cl-defun doom/counsel-css--open-brace-forward (&optional $bound)
  "Move to next open brace, skip commented brace"
  (interactive)
  (let ($ret)
    (setq $ret (re-search-forward "[^#]{" $bound t))
    (unless $ret (cl-return-from doom/counsel-css--open-brace-forward nil))
    (backward-char)
    (if (doom/counsel-css--comment-p (point))
        (doom/counsel-css--open-brace-forward $bound)
      $ret)))

(defun doom/counsel-css--substr-last-string ($text $key)
  "Return the tail of $text without $key strings"
  (while (string-match $key $text)
    (setq $text (substring $text (1+ (string-match $key $text)))))
  $text)

(cl-defun doom/counsel-css--fetch-previous-line (&optional $prev $noexcursion)
  "Return previous nth ($prev) line strings.
If $noexcursion is not-nil cursor doesn't move."
  ;; In compressed Css without this return, it takes long time
  (if (eq 1 (line-number-at-pos))
      (cl-return-from doom/counsel-css--fetch-previous-line ""))
  (or $prev (setq $prev 1))
  (if $noexcursion (setq $noexcursion (point)))
  (move-beginning-of-line (- 1 $prev))
  (let (($po (point)) $res)
    (move-end-of-line 1)
    (setq $res (buffer-substring-no-properties $po (point)))
    (if $noexcursion (goto-char $noexcursion))
    $res))

(defun doom/counsel-css--comment-p (&optional $pos)
  (or $pos (setq $pos (point)))
  (nth 4 (parse-partial-sexp (point-min) $pos)))

(cl-defun doom/counsel-css--extract-selector ()
  "Return selector infomation at the point"
  (let (($multi "") $s $po1 $po2 $po3 $str $commentp)
    ;; Collect multiple selector across previous lines
    ;; (i.e. "div, \n p, \n span {...}")
    (save-excursion
      (while (string-match ",[\s\t]*$"
                           (setq $s (doom/counsel-css--fetch-previous-line)))
        ;; Skip commented selector (i.e. " // .blue,")
        (save-excursion
          (move-beginning-of-line 1)
          (setq $po3 (point))
          (setq $commentp (doom/counsel-css--comment-p (search-forward ","))))
        (unless $commentp
          (setq $multi (format "%s %s" (string-trim $s) $multi)))))
    ;; Extract selector include one-line-nesting (i.e. "div { p {...} }")
    (save-excursion
      (skip-chars-backward "^{};\n")
      (setq $po1 (point))
      ;; (setq $beg2 $po1)
      (skip-chars-forward "^{")
      (setq $po2 (point))
      (setq $str (buffer-substring-no-properties $po1 $po2))
      ;; i.e. "div { .box { p"  ->  " p"
      (setq $str (doom/counsel-css--substr-last-string $str "{\\|}"))
      (setq $str (string-trim $str))
      ;; Return (selector-name . (selector-beginning-point . selector-end-point))
      (if (equal $multi "")
          (cons (format "%s" $str) (cons $po1 $po2))
        (cons (format "%s %s" (string-trim $multi) $str)
              (cons $po3 $po2))))))

(cl-defun doom/counsel-css--selector-next (&optional $bound)
  "Return and goto next selector."
  (unless (doom/counsel-css--open-brace-forward $bound)
    (cl-return-from doom/counsel-css--selector-next nil))
  (doom/counsel-css--extract-selector))

(defun doom/counsel-css--selector-to-hash (&optional no-line-numbers)
  "Collect all selectors and make hash table"
  (let ($selector $paren-beg $paren-end $hash $dep $max $sl
                  $selector-name $selector-beg $selector-end
                  $selector-line)
    (setq $hash (make-hash-table :test 'equal))
    (save-excursion
      (goto-char (point-min))
      (while (setq $selector (doom/counsel-css--selector-next))
        (setq $paren-beg (point))
        (setq $paren-end (scan-sexps $paren-beg 1))
        (setq $max (cons $paren-end $max))
        (setq $max (mapcar (lambda ($p) (if (< $p $paren-beg) nil $p)) $max))
        (setq $max (delq nil $max))
        (setq $dep (length $max))
        (setq $selector-name (car $selector))
        (setq
         $selector-name
         (cl-case $dep
           (1 (propertize $selector-name 'face 'doom/counsel-css-selector-depth-face-1))
           (2 (propertize $selector-name 'face 'doom/counsel-css-selector-depth-face-2))
           (3 (propertize $selector-name 'face 'doom/counsel-css-selector-depth-face-3))
           (4 (propertize $selector-name 'face 'doom/counsel-css-selector-depth-face-4))
           (5 (propertize $selector-name 'face 'doom/counsel-css-selector-depth-face-5))
           (6 (propertize $selector-name 'face 'doom/counsel-css-selector-depth-face-6))))
        (setq $selector-beg (cadr $selector))
        (setq $selector-end (cddr $selector))
        (setq $selector-line (line-number-at-pos $selector-beg))
        (if (<= $dep (length $sl))
            (cl-loop repeat (- (1+ (length $sl)) $dep) do (pop $sl)))
        (setq $sl (cons $selector-name $sl))
        (puthash
         (if no-line-numbers
             (mapconcat 'identity (reverse $sl) " ")
           (format "%s: %s"
                   (propertize (number-to-string $selector-line)
                               'face 'font-lock-function-name-face)
                   (mapconcat 'identity (reverse $sl) " ")))
         (list $paren-beg $paren-end $dep $selector-beg $selector-end $selector-line)
         $hash)))
    $hash))

(defun doom/counsel-css--imenu-create-index-function ()
  (let (($hash (doom/counsel-css--selector-to-hash t)))
    (cl-loop for $k being hash-key in $hash using (hash-values $v)
             collect (cons $k $v))))

;;;###autoload
(defun doom|counsel-css-imenu-setup ()
  (when (memq major-mode '(css-mode scss-mode less-css-mode))
    (setq imenu-create-index-function 'doom/counsel-css--imenu-create-index-function)))

;;;###autoload
(defun doom/counsel-css ()
  (interactive)
  (require 'counsel)
  (ivy-read "Selectors: "
            (let (($hash (doom/counsel-css--selector-to-hash)))
              (cl-loop for $k being hash-key in $hash using (hash-values $v)
                       collect (cons $k $v)))
            :require-match t
            :caller 'doom/counsel-css))

(provide 'defuns-scss)
;;; defuns-scss.el ends here
