;;; emacs/eww/autoload.el -*- lexical-binding: t; -*-

;; NOTE: Many of these functions were adapted from Protesilaos Stavrou's
;;   dotfiles. See https://protesilaos.com/codelog/2021-03-25-emacs-eww

;; Adapted from `prot-eww-jump-to-url-on-page'
(defun eww--capture-urls-on-page (&optional position)
  "Capture all the links on the current web page.

Return a list of strings. Strings are in the form \"LABEL @ URL\".
When optional argument POSITION is non-nil, include position info in the strings
too, so strings take the form: \"POSITION ~ LABEL @ URL\"."
  (let (links match)
    (save-excursion
      (goto-char (point-max))
      (while (setq match (text-property-search-backward 'shr-url))
        (let* ((raw-url (prop-match-value match))
               (start-point-prop (prop-match-beginning match))
               (end-point-prop (prop-match-end match))
               (url (when (stringp raw-url)
                      (propertize raw-url 'face 'link)))
               (label (replace-regexp-in-string
                       "\n" " " ; NOTE 2021-07-25: newlines break completion
                       (buffer-substring-no-properties
                        start-point-prop end-point-prop)))
               (point start-point-prop)
               (line (line-number-at-pos point t))
               (column (save-excursion (goto-char point) (current-column)))
               (coordinates (propertize
                             (format "%d,%d (%d)" line column point)
                             'face 'shadow)))
          (when url
            (push (if position
                      (format "%-15s ~ %s  @ %s" coordinates label url)
                    (format "%s  @ %s" label url))
                  links)))))
    links))

(defun eww--capture-headings-on-page ()
  "Return an alist in the form \"LABEL . POINT\" for the current buffer."
  (let ((heading-stack '())
        headings match)
    (save-excursion
      (goto-char (point-min))
      (while (setq match (text-property-search-forward 'outline-level))
        (let* ((level (prop-match-value match))
               (start-point-prop (prop-match-beginning match))
               (end-point-prop (prop-match-end match))
               (text (replace-regexp-in-string
                      "\n" " "      ; NOTE 2021-07-25: newlines break completion
                      (buffer-substring-no-properties
                       start-point-prop end-point-prop))))
          (cond
           ((= level (length heading-stack))
            (pop heading-stack)
            (push text heading-stack))
           ((< level (length heading-stack))
            ;; There's an upward gap between headings (for example: h5, then h2)
            (dotimes (_ (1+ (- (length heading-stack) level)))
              (pop heading-stack))
            (push text heading-stack))
           ((> level (length heading-stack))
            ;; There's a downward gap between headings (for example: h2, then h5)
            (dotimes (_ (1- (- level (length heading-stack))))
              (push nil heading-stack))
            (push text heading-stack)))
          (push (cons
                 (concat
                  (let ((preceeding-heading-stack (remove nil (cdr heading-stack))))
                    (when preceeding-heading-stack
                      (propertize
                       (concat
                        (string-join (reverse preceeding-heading-stack) "/")
                        "/")
                       'face 'shadow)))
                  (car heading-stack))
                 start-point-prop)
                headings))))
    headings))

;; Adapted from `prot-eww--rename-buffer'
(defun +eww-page-title-or-url (&rest _)
  (let ((prop (if (string-empty-p (plist-get eww-data :title)) :url :title)))
    (format "*%s # eww*" (plist-get eww-data prop))))


;;
;;; Commands

;; Adapted from `prot-eww-quit'
;;;###autoload
(defun +eww/quit ()
  "Quit eww and kill all its buffers."
  (interactive nil 'eww-mode)
  (when (yes-or-no-p "Are you sure you want to quit eww?")
    (save-match-data
      (cl-loop with case-fold-search = t
               for buf in (doom-buffer-list)
               if (with-current-buffer buf
                    (or (eq major-mode 'eww-mode)
                        (and (derived-mode-p 'special-mode)
                             (string-match "\\*.*eww.*\\*" (buffer-name)))))
               do (kill-buffer buf)))))

;; Adapted from `prot-eww-jump-to-url-on-page'
;;;###autoload
(defun +eww/jump-to-url-on-page (&optional arg)
  "Jump to URL position on the page using completion.

When called without ARG (\\[universal-argument]) get URLs only
from the visible portion of the buffer.  But when ARG is provided
consider whole buffer."
  (interactive "P" 'eww-mode)
  (unless (derived-mode-p 'eww-mode)
    (user-error "Not in an eww buffer!"))
  (let* ((links
          (if arg
              (eww--capture-urls-on-page t)
            (save-restriction
              (if (use-region-p)
                  (narrow-to-region (region-beginning) (region-end))
                (narrow-to-region (window-start) (window-end)))
              (eww--capture-urls-on-page t))))
         (prompt-scope (if arg
                           (propertize "URL on the page" 'face 'warning)
                         "visible URL"))
         (prompt (format "Jump to %s: " prompt-scope))
         (selection (completing-read prompt links nil t))
         (position (replace-regexp-in-string "^.*(\\([0-9]+\\))[\s\t]+~" "\\1" selection))
         (point (string-to-number position)))
    (goto-char point)
    (recenter)))

;; Adapted from `prot-eww-jump-to-url-on-page'
;;;###autoload
(defun +eww/jump-to-heading-on-page ()
  "Jump to heading position on the page (whole buffer) using completion."
  (interactive nil 'eww-mode)
  (unless (derived-mode-p 'eww-mode)
    (user-error "Not in an eww buffer!"))
  (let* ((headings (eww--capture-headings-on-page))
         (selection (completing-read "Jump to heading: " headings nil t)))
    (goto-char (alist-get selection headings nil nil #'string=))
    (recenter)))

;; Adapted from `prot-eww-open-in-other-window'
;;;###autoload
(defun +eww/open-in-other-window ()
  "Use `eww-open-in-new-buffer' in another window."
  (interactive nil 'ewe-mode)
  (other-window-prefix)
  (eww-open-in-new-buffer))

;;;###autoload
(defun +eww/copy-current-url ()
  "Copy the open page's URL to the kill ring."
  (interactive nil 'eww-mode)
  (let ((url (eww-current-url)))
    (kill-new url)
    (message url)))

;;;###autoload
(defun +eww/increase-font-size ()
  "Increase the font size in `eww-mode'."
  (interactive nil 'eww-mode)
  (if shr-use-fonts
      (let* ((cur (face-attribute 'shr-text :height nil))
             (cur (if (floatp cur) cur 1.0)))
        (set-face-attribute 'shr-text nil :height (+ cur 0.1)))
    (text-scale-increase 0.5)))

;;;###autoload
(defun +eww/decrease-font-size ()
  "Decrease the font size in `eww-mode'."
  (interactive nil 'eww-mode)
  (if shr-use-fonts
      (let* ((cur (face-attribute 'shr-text :height nil))
             (cur (if (floatp cur) cur 1.0)))
        (set-face-attribute 'shr-text nil :height (- cur 0.1)))
    (text-scale-decrease 0.5)))
