;;; tools/cloudformation/config.el -*- lexical-binding: t; -*-

(when (featurep! :lang javascript)
  (define-derived-mode cfn-json-mode js-mode
    "CFN-JSON"
    "Simple mode to edit CloudFormation template in JSON format."
    (setq-local js-indent-level 2))

  (add-to-list 'magic-mode-alist
               '("\\({\n *\\)? *[\"']AWSTemplateFormatVersion" . cfn-json-mode)))

(when (featurep! :lang yaml)
  (define-derived-mode cfn-yaml-mode yaml-mode
    "CFN-YAML"
    "Simple mode to edit CloudFormation template in YAML format.")

  (add-to-list 'magic-mode-alist
               '("\\(---\n\\)?AWSTemplateFormatVersion:" . cfn-yaml-mode)))

(when (featurep! :checkers syntax)
  (flycheck-define-checker cfn-lint
    "AWS CloudFormation linter using cfn-lint.
     Install cfn-lint first: pip install cfn-lint
     See `https://github.com/aws-cloudformation/cfn-python-lint'."

    :command ("cfn-lint" "-f" "parseable" source)
    :error-patterns ((warning line-start (file-name) ":" line ":" column
                              ":" (one-or-more digit) ":" (one-or-more digit) ":"
                              (id "W" (one-or-more digit)) ":" (message) line-end)
                     (error line-start (file-name) ":" line ":" column
                            ":" (one-or-more digit) ":" (one-or-more digit) ":"
                            (id "E" (one-or-more digit)) ":" (message) line-end))
    :modes (cfn-json-mode cfn-yaml-mode))

  (add-to-list 'flycheck-checkers 'cfn-lint)

  (when (featurep! :lang javascript)
    (add-hook 'cfn-json-mode-hook 'flycheck-mode))

  (when (featurep! :lang yaml)
    (add-hook 'cfn-yaml-mode-hook 'flycheck-mode)))
