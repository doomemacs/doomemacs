;;; emacs/eww/autoload.el -*- lexical-binding: t; -*-

;; a bunch of these things were borrowed from: Protesilaos Stavrou and remodeled for Doom
;; https://protesilaos.com/codelog/2021-03-25-emacs-eww

;;;###autoload
(defun eww--rename-buffer ()
  "Rename EWW buffer using page title or URL.
To be used by `eww-after-render-hook'."
  (let ((name (if (eq "" (plist-get eww-data :title))
                  (plist-get eww-data :url)
                (plist-get eww-data :title))))
    (rename-buffer (format "*%s # eww*" name) t)))

(defun eww--capture-url-on-page (&optional position)
  "Capture all the links on the current web page.

Return a list of strings.  Strings are in the form LABEL @ URL.
When optional argument POSITION is non-nil, include position info
in the strings too, so strings take the form
LABEL @ URL ~ POSITION."
  (let (links match)
    (save-excursion
      (goto-char (point-max))
      ;; NOTE 2021-07-25: The first clause in the `or' is meant to
      ;; address a bug where if a URL is in `point-min' it does not get
      ;; captured.
      (while (setq match (text-property-search-backward 'shr-url))
        (let* ((raw-url (prop-match-value match))
               (start-point-prop (prop-match-beginning match))
               (end-point-prop (prop-match-end match))
               (url (when (stringp raw-url)
                      (propertize raw-url 'face 'link)))
               (label (replace-regexp-in-string "\n" " " ; NOTE 2021-07-25: newlines break completion
                                                (buffer-substring-no-properties
                                                 start-point-prop end-point-prop)))
               (point start-point-prop)
               (line (line-number-at-pos point t))
               (column (save-excursion (goto-char point) (current-column)))
               (coordinates (propertize
                             (format "%d,%d (%d)" line column point)
                             'face 'shadow)))
          (when url
            (if position
                (push (format "%-15s ~ %s  @ %s"
                              coordinates label url)
                      links)
              (push (format "%s  @ %s"
                            label url)
                    links))))))
    links))

(defmacro eww-act-visible-window (&rest body)
  "Run BODY within narrowed-region.
If region is active run BODY within active region instead.
Return the value of the last form of BODY."
  `(save-restriction
     (if (use-region-p)
         (narrow-to-region (region-beginning) (region-end))
       (narrow-to-region (window-start) (window-end)))
     ,@body))

;;;###autoload
(defun +eww-jump-to-url-on-page (&optional arg)
  "Jump to URL position on the page using completion.

When called without ARG (\\[universal-argument]) get URLs only
from the visible portion of the buffer.  But when ARG is provided
consider whole buffer."
  (interactive "P")
  (when (derived-mode-p 'eww-mode)
    (let* ((links
            (if arg
                (eww--capture-url-on-page t)
              (eww-act-visible-window
               (eww--capture-url-on-page t))))
           (prompt-scope (if arg
                             (propertize "URL on the page" 'face 'warning)
                           "visible URL"))
           (prompt (format "Jump to %s: " prompt-scope))
           (selection (completing-read prompt links nil t))
           (position (replace-regexp-in-string "^.*(\\([0-9]+\\))[\s\t]+~" "\\1" selection))
           (point (string-to-number position)))
      (goto-char point)
      (recenter))))

;;;###autoload
(defun +eww-open-in-other-window ()
  "Use `eww-open-in-new-buffer' in another window."
  (interactive)
  (other-window-prefix)       ; For emacs28 -- it's a hack, but why not?
  (eww-open-in-new-buffer))

;;;###autoload
(defun +eww-copy-current-url ()
  (interactive)
  (let ((url (eww-current-url)))
    (kill-new url)
    (message url)))

;;;###autoload
(defun +eww-increase-font-size ()
  (interactive)
  (if shr-use-fonts
      (let* ((cur (face-attribute 'shr-text :height nil))
             (cur (if (floatp cur) cur 1.0)))
        (set-face-attribute 'shr-text nil :height (+ cur 0.1)))
    (text-scale-increase 0.5)))

;;;###autoload
(defun +eww-decrease-font-size ()
  (interactive)
  (if shr-use-fonts
      (let* ((cur (face-attribute 'shr-text :height nil))
             (cur (if (floatp cur) cur 1.0)))
        (set-face-attribute 'shr-text nil :height (- cur 0.1)))
    (text-scale-decrease 0.5)))
