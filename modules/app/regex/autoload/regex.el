;;; app/regex/autoload/regex.el

(defvar +regex--text-buffer nil)
(defvar +regex--text-replace-buffer nil)
(defvar +regex--expr-buffer nil)
(defvar +regex--expr-replace-buffer nil)
(defvar +regex--groups-buffer nil)
(defvar +regex--replace-buffer nil)

;;
(defface +regex-match-0-face
  `((t (:foreground "Black" :background ,(doom-color 'magenta) :bold t)))
  "TODO"
  :group 'doom)

(defface +regex-match-1-face
  `((t (:foreground "Black" :background ,(doom-color 'blue) :bold t)))
  "TODO"
  :group 'doom)

(defface +regex-match-2-face
  `((t (:foreground "Black" :background ,(doom-color 'green) :bold t)))
  "TODO"
  :group 'doom)

(defvar +regex-faces
  '(+regex-match-0-face +regex-match-1-face +regex-match-2-face)
  "TODO")

;;
(defvar +regex-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" #'+regex-update-buffers)
    (define-key map "\C-c\C-r" #'=regex/replace)
    (define-key map "\C-c\C-k" #'+regex/quit)
    (define-key map [remap doom-kill-buffer]      #'+regex/quit)
    (define-key map [remap doom/kill-this-buffer] #'+regex/quit)
    (define-key map [remap kill-this-buffer]      #'+regex/quit)
    (define-key map [remap kill-buffer]           #'+regex/quit)
    map)
  "TODO")

;;;###autoload
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
  (unless (buffer-live-p +regex--expr-buffer)
    (condition-case ex
        (progn
          (setq +regex--expr-buffer (get-buffer-create "*doom-regex*")
                +regex--text-buffer (if dummy-text (get-buffer-create "*doom-regex-text*") (current-buffer))
                +regex--groups-buffer (get-buffer-create "*doom-regex-groups*"))
          (when dummy-text
            (+workspace-switch +regex-workspace-name t)
            (switch-to-buffer +regex--text-buffer)
            (with-current-buffer +regex--text-buffer
              (insert +regex-dummy-text)))
          (doom-popup-buffer +regex--groups-buffer)
          (doom-popup-buffer +regex--expr-buffer)
          (with-current-buffer +regex--expr-buffer
            (conf-mode)
            (rainbow-delimiters-mode +1)
            (doom/toggle-line-numbers +1)
            (setq-local require-final-newline nil)
            (+regex-mode +1)
            (text-scale-set 3)))
      ('error
       (+regex/quit)
       (error "Failed to open the Regexp IDE: %s" ex)))))

;;;###autoload
(defun =regex/replace ()
  (interactive)
  (unless (buffer-live-p +regex--replace-buffer)
    (let (text)
      (=regex t)
      (with-selected-window (get-buffer-window +regex--text-buffer)
        (setq text (buffer-string))
        (select-window (split-window-right))
        (switch-to-buffer (get-buffer-create "*doom-regex-text-repl*"))
        (erase-buffer)
        (insert text)
        (read-only-mode +1)
        (setq +regex--text-replace-buffer (current-buffer)))
      (with-current-buffer +regex--expr-buffer
        (select-window (split-window-right))
        (switch-to-buffer (get-buffer-create "*doom-regex-repl*"))
        (conf-mode)
        (rainbow-delimiters-mode +1)
        (doom/toggle-line-numbers -1)
        (setq-local require-final-newline nil)
        (setq mode-line-format nil
              +regex--expr-replace-buffer (current-buffer))
        (+regex-mode +1)
        (text-scale-set 3)))))

;;;###autoload
(defun +regex/quit ()
  "TODO"
  (interactive)
  (when (and +regex--text-buffer (buffer-live-p +regex--text-buffer))
    (with-current-buffer +regex--text-buffer
      (+regex-mode -1)
      (remove-overlays nil nil 'category '+regex))
    (when (equal (buffer-name +regex--text-buffer) "*doom-regex-text*")
      (kill-buffer +regex--text-buffer)))
  (when (equal (+workspace-current-name) +regex-workspace-name)
    (+workspace/delete +regex-workspace-name))
  (mapc (lambda (bufname)
          (let ((buf (symbol-value bufname)))
            (when (and buf (buffer-live-p buf))
              (kill-buffer buf)
              (set bufname nil))))
        (list '+regex--text-replace-buffer
              '+regex--expr-replace-buffer
              '+regex--expr-buffer
              '+regex--groups-buffer
              '+regex--replace-buffer)))

(defun +regex--expr ()
  (when (buffer-live-p +regex--expr-buffer)
    (with-current-buffer +regex--expr-buffer
      (string-trim (buffer-string)))))

(defun +regex--expr-replace ()
  (when (buffer-live-p +regex--expr-replace-buffer)
    (with-current-buffer +regex--expr-replace-buffer
      (string-trim (buffer-string)))))

;;;###autoload
(defun +regex-update-buffers (&optional beg end len)
  (interactive)
  (let* ((inhibit-read-only t)
         (regex (or (+regex--expr) ""))
         (replace (or (+regex--expr-replace) ""))
         (str (or (with-current-buffer +regex--text-buffer (buffer-string)) "")))
    (with-current-buffer +regex--groups-buffer
      (erase-buffer))
    (with-current-buffer +regex--text-buffer
      (remove-overlays nil nil 'category '+regex)
      (when (> (length regex) 0)
        (save-excursion
          (goto-char (point-min))
          (pcase +regex-default-backend
            ('emacs (+regex-backend-emacs regex replace str))
            ('perl  (+regex-backend-perl regex replace str))))))
    (with-current-buffer +regex--groups-buffer
      (goto-char (point-min)))))


;; --- backends ---------------------------

(defun +regex--render-perl (regex text)
  "From <https://github.com/jwiegley/regex-tool>"
  (with-temp-buffer
    (insert (format "@lines = <DATA>;
$line = join(\"\", @lines);
print \"(\";
while ($line =~ m/%s/gm) {
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
%s" (replace-regexp-in-string "/" "\\/" regex nil t) text))
    (call-process-region (point-min) (point-max) "perl" t t)
    (goto-char (point-min))
    (read (current-buffer))))

(defun +regex--replace-perl (regex replace text)
  (unless (or (string-empty-p regex)
              (string-empty-p replace)
              (string-empty-p text))
    (with-temp-buffer
      (insert (format "@lines = <DATA>;
$line = join(\"\", @lines);
$line =~ s/%s/%s/gm;
print $line;
__DATA__
%s" (replace-regexp-in-string "/" "\\/" regex nil t) (replace-regexp-in-string "/" "\\/" replace nil t) text))
      (call-process-region (point-min) (point-max) "perl" t t)
      (buffer-string))))

;;;###autoload
(defun +regex-backend-perl (regex replace str)
  "TODO"
  (cl-assert (stringp regex))
  (cl-assert (stringp replace))
  (cl-assert (stringp str))
  (let ((i 0)
        (results (+regex--render-perl regex str)))
    (when (buffer-live-p +regex--text-replace-buffer)
      (let ((replace (+regex--replace-perl regex replace str)))
        (with-current-buffer +regex--text-replace-buffer
          (erase-buffer)
          (insert
           (if (and (listp results)
                    replace
                    (not (string-empty-p replace)))
               replace
             str)))))
    (dolist (result (if (listp results) results))
      (let* ((offset (nth 0 result))
             (length (nth 1 result))
             (matches (nthcdr 2 result))
             (ov (make-overlay (1+ offset) (+ offset length 1))))
        (overlay-put ov 'face (nth (mod i 3) +regex-faces))
        (overlay-put ov 'category '+regex)
        (cl-incf i)
        (let* ((match-zero (buffer-substring (1+ offset) (+ offset length 1)))
               (line (format "Match: %s\n" (propertize match-zero 'face 'font-lock-string-face))))
          (with-current-buffer +regex--groups-buffer
            (insert line)))
        (dolist (match matches)
          (with-current-buffer +regex--groups-buffer
            (goto-char (point-max))
            (insert (format "Group %d: %s\n"
                            (propertize (car match) 'face 'font-lock-constant-face)
                            (propertize (cdr match) 'face 'font-lock-string-face)))))
        (with-current-buffer +regex--groups-buffer
          (insert ?\n))))))

;;;###autoload
(defun +regex-backend-emacs (regex replace str)
  "TODO"
  (cl-assert (stringp regex))
  (cl-assert (stringp replace))
  (cl-assert (stringp str))
  (let ((i 0)
        pos)
    (when (buffer-live-p +regex--text-replace-buffer)
      (with-current-buffer +regex--text-replace-buffer
        (erase-buffer)
        (insert str)
        (when (and (listp results) (string-empty-p replace))
          (replace-regexp regex replace))))
    (while (and (setq pos (point))
                (re-search-forward regex nil t))
      (if (= (point) pos)
          (forward-char 1)
        (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
          (overlay-put ov 'face (nth (mod i 3) +regex-faces))
          (overlay-put ov 'category '+regex))
        (cl-incf i)
        (dotimes (i 10)
          (when-let (text (match-string i))
            (save-match-data
              (with-current-buffer +regex--groups-buffer
                (goto-char (point-max))
                (insert
                 (format "Group %d: %s\n"
                         (propertize i    'face 'font-lock-constant-face)
                         (propertize text 'face 'font-lock-string-face)))))))
        (with-current-buffer +regex--groups-buffer
          (insert ?\n))))))

