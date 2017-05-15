;;; app/regex/autoload.el

(defvar +regex--text-buffer nil)
(defvar +regex--expr-buffer nil)
(defvar +regex--groups-buffer nil)

;;
(defface +regex-match-0-face
  '((t (:foreground "Black" :background "Red" :bold t)))
  ""
  :group 'doom)

(defface +regex-match-1-face
  '((t (:foreground "Black" :background "Blue" :bold t)))
  ""
  :group 'doom)

(defface +regex-match-2-face
  '((t (:foreground "Black" :background "Green" :bold t)))
  ""
  :group 'doom)

(defvar +regex-faces
  '(+regex-match-0-face +regex-match-1-face +regex-match-2-face)
  "TODO")

;;
(defvar +regex-mode-map
  (let ((map (make-sparse-keymap)))
    (evil-define-key* 'normal map
      "\C-c\C-c" #'+regex-update-buffers
      "\C-c\C-k" #'+regex/quit)
    map)
  "TODO")

(define-minor-mode +regex-mode
  "TODO"
  :init-value nil
  :global nil
  :lighter ""
  :keymap +regex-mode-map
  (if +regex-mode
      (add-hook 'after-change-functions #'+regex-update-buffers nil t)
    (remove-hook 'after-change-functions #'+regex-update-buffers t)))

;;;###autoload
(defun =regex (&optional dummy-text)
  "Start the Regex IDE."
  (interactive "P")
  (condition-case ex
      (progn
        (setq +regex--expr-buffer (get-buffer-create "*doom-regex*")
              +regex--text-buffer (if dummy-text (get-buffer-create "*doom-regex-text*") (current-buffer))
              +regex--groups-buffer (get-buffer-create "*doom-regex-groups*"))
        (when dummy-text
          (+workspace-switch +regex-workspace-name t)
          (switch-to-buffer +regex--text-buffer)
          (insert +regex-dummy-text))
        (doom-popup-buffer +regex--groups-buffer)
        (doom-popup-buffer +regex--expr-buffer)
        (with-current-buffer +regex--expr-buffer
          (conf-mode)
          (doom-buffer-mode +1)
          (rainbow-delimiters-mode +1)
          (linum-mode -1)
          (+regex-mode +1)
          (text-scale-set 3)))
    ('error
     (+regex/quit)
     (error "Failed to open the Regexp IDE: %s" ex))))

;;;###autoload
(defun +regex/quit ()
  "TODO"
  (interactive)
  (kill-buffer +regex--expr-buffer)
  (kill-buffer +regex--groups-buffer)
  (when (and +regex--text-buffer (buffer-live-p +regex--text-buffer))
    (with-current-buffer +regex--text-buffer
      (+regex-mode -1)
      (remove-overlays)))
  (setq +regex--expr-buffer nil
        +regex--text-buffer nil
        +regex--groups-buffer nil)
  (when (equal (+workspace-current-name) +regex-workspace-name)
    (kill-buffer +regex--text-buffer)
    (+workspace/delete +regex-workspace-name)))

;;;###autoload
(defun +regex-update-buffers (&optional beg end len)
  (interactive)
  (let ((regex (with-current-buffer +regex--expr-buffer (buffer-string))))
    (when (> (length regex) 0)
      (with-current-buffer +regex--groups-buffer
        (erase-buffer))
      (with-current-buffer +regex--text-buffer
        (remove-overlays)
        (save-excursion
          (ignore-errors
            (goto-char (point-min))
            (pcase +regex-default-backend
              ('emacs (+regex-backend-emacs regex))
              ('perl  (+regex-backend-perl regex))))))
      (with-current-buffer +regex--groups-buffer
        (goto-char (point-min))))))


;; --- backends ---------------------------

(defun +regex--render-perl (regex sample)
  "From <https://github.com/jwiegley/regex-tool>"
  (with-temp-buffer
    (unless (string-match-p "^/.+/[gm]*$" regex)
      (setq regex (concat "/" regex "/g")))
    (insert (format "@lines = <DATA>;
$line = join(\"\", @lines);
print \"(\";
while ($line =~ m%smg) {
  print \"(\", length($`), \" \", length($&), \" \";
  for $i (1 .. 20) {
    if ($$i) {
      my $group = $$i;
      $group =~ s/([\\\\\"])/\\\\\\1/g;
      print \"(\", $i, \" . \\\"\", $group, \"\\\") \";
    }
  }
  print \")\";
}
print \")\";
__DATA__
%s" regex sample))
    (call-process-region (point-min) (point-max) "perl" t t)
    (goto-char (point-min))
    (read (current-buffer))))

;;;###autoload
(defun +regex-backend-perl (regex)
  "TODO"
  (let ((results (+regex--render-perl regex (buffer-string)))
        (i 0))
    (dolist (result results)
      (let ((offset (nth 0 result))
            (length (nth 1 result))
            (matches (nthcdr 2 result)))
        (overlay-put (make-overlay (1+ offset) (+ offset length 1))
                     'face (nth (mod i 3) +regex-faces))
        (cl-incf i)
        (let ((match-zero (buffer-substring (1+ offset) (+ offset length 1))))
          (with-current-buffer +regex--groups-buffer
            (insert (format "Group 0: '%s'\n" match-zero))))
        (dolist (match matches)
          (with-current-buffer +regex--groups-buffer
            (goto-char (point-max))
            (insert (format "Group %d: '%s'\n" (car match)
                            (cdr match)))))
        (with-current-buffer +regex--groups-buffer
          (insert ?\n))))))

;;;###autoload
(defun +regex-backend-emacs (regex)
  "TODO"
  (let ((i 0)
        pos)
    (while (and (setq pos (point))
                (re-search-forward regex nil t))
      (if (= (point) pos)
          (forward-char 1)
        (overlay-put (make-overlay (match-beginning 0)
                                   (match-end 0))
                     'face (nth (mod i 3) +regex-faces))
        (cl-incf i)
        (dotimes (i 10)
          (when-let (text (match-string i))
            (save-match-data
              (with-current-buffer +regex--groups-buffer
                (goto-char (point-max))
                (insert (format "Group %d: '%s'\n" i text))))))
        (with-current-buffer +regex--groups-buffer
          (insert ?\n))))))

