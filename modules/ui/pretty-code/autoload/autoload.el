;;; ui/pretty-code/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defvar +pretty-code-enabled-modes t
  "List of major modes in which `prettify-symbols-mode' should be enabled.
If t, enable it everywhere. If the first element is 'not, enable it in any mode
besides what is listed.")


;;;###autoload
(defun +pretty-code|init-pretty-symbols ()
  "Enabled `prettify-symbols-mode'.

If the current major mode is disabled in `+pretty-code-enabled-modes', this
function does nothing. Otherwise, it sets the value of
`prettify-code-symbols-alist' according to `+pretty-code-symbols-alist' for the
current major mode."
  (when (or (eq +pretty-code-enabled-modes 't)
            (if (eq (car +pretty-code-enabled-modes 'not))
                (not (memq major-mode (cdr +pretty-code-enabled-modes)))
              (memq major-mode +pretty-code-enabled-modes)))
    (setq prettify-symbols-alist
          (append (alist-get major-mode +pretty-code-symbols-alist)
                  (default-value 'prettify-symbols-alist)))
    (when prettify-symbols-mode
      (prettify-symbols-mode -1))
    (prettify-symbols-mode +1)))
