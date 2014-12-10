;;; flycheck-objc.el --- Flycheck for objc-mode.     -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Rafal Kowalski

;; Author: Rafal Kowalski <rafal.kowalski@mac.com>
;; Keywords: c, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Flycheck settings for objc-mode.

;;; Code:

(require 'flycheck)

(flycheck-def-option-var flycheck-objc-clang-definitions nil objc-clang
  "Additional preprocessor definitions for Clang.

The value of this variable is a list of strings, where each
string is an additional definition to pass to Clang, via the `-D'
option."
  :type '(repeat (string :tag "Definition"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.15"))

(flycheck-def-option-var flycheck-objc-clang-include-path nil objc-clang
  "A list of include directories for Clang.

Thae value of this variable is a list of strings, where each
string is a directory to add to the include path of Clang.
Relative paths are relative to the file being checked."
  :type '(repeat (directory :tag "Include directory"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.14"))

(flycheck-def-option-var flycheck-objc-clang-framework-path nil objc-clang
  "A list of frameworks for Clang.

Thae value of this variable is a list of strings, where each
string is a path to a frameworks directory to add to the frameworks
path of Clang.  Relative paths are relative to the file being
checked."
  :type '(repeat (directory :tag "Framework directory"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.14"))

(flycheck-def-option-var flycheck-objc-clang-includes nil objc-clang
  "A list of additional include files for Clang.

The value of this variable is a list of strings, where each
string is a file to include before syntax checking.  Relative
paths are relative to the file being checked."
  :type '(repeat (file :tag "Include file"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.15"))

(flycheck-def-option-var flycheck-objc-clang-language-standard nil objc-clang
  "The language standard to use in Clang.

The value of this variable is either a string denoting a language
standard, or nil, to use the default standard.  When non-nil,
pass the language standard via the `-std' option."
  :type '(choice (const :tag "Default standard" nil)
                 (string :tag "Language standard"))
  :safe #'stringp
  :package-version '(flycheck . "0.15"))

(flycheck-def-option-var flycheck-objc-clang-standard-library nil objc-clang
  "The standard library to use for Clang.

The value of this variable is the name of a standard library as
string, or nil to use the default standard library.

Refer to the Clang manual at URL
`http://clang.llvm.org/docs/UsersManual.html' for more
information about the standard library."
  :type '(choice (const "libc++")
                 (const :tag "GNU libstdc++" "libstdc++")
                 (string :tag "Library name"))
  :safe #'stringp
  :package-version '(flycheck . "0.15"))

(flycheck-def-option-var flycheck-objc-clang-archs nil objc-clang
  "What architectures to use for clang.

When non-nil, set the architectures, via `-arch'."
  :type '(repeat (file :tag "Architecture"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.15"))

(flycheck-def-option-var flycheck-objc-clang-sysroot nil objc-clang
  "The system root to use in clang.

When non-nil,pass the language standard via the `-isysroot' option."
  :type '(choice (const :tag "Default sysroot" nil)
                 (string :tag "Sysroot"))
  :safe #'stringp
  :package-version '(flycheck . "0.15"))

(flycheck-def-option-var flycheck-objc-clang-warnings '("all" "extra") objc-clang
  "A list of additional warnings to enable in Clang.

The value of this variable is a list of strings, where each string
is the name of a warning category to enable.  By default, all
recommended warnings and some extra warnings are enabled (as by
`-Wall' and `-Wextra' respectively).

Refer to the Clang manual at URL
`http://clang.llvm.org/docs/UsersManual.html' for more
information about warnings."
  :type '(choice (const :tag "No additional warnings" nil)
                 (repeat :tag "Additional warnings"
                         (string :tag "Warning name")))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.14"))

(flycheck-define-checker objc-clang
  "A objc syntax checker using Clang.

See URL `http://clang.llvm.org/'."
  :command ("clang"
            "-fsyntax-only"
            "-fno-color-diagnostics"    ; Do not include color codes in output
            "-fno-caret-diagnostics"    ; Do not visually indicate the source
                                        ; location
            "-fno-diagnostics-show-option" ; Do not show the corresponding
                                        ; warning group
            (option "-isysroot" flycheck-objc-clang-sysroot)
            (option-list "-arch" flycheck-objc-clang-archs)
            (option "-std=" flycheck-objc-clang-language-standard)
            (option "-stdlib=" flycheck-objc-clang-standard-library)
            (option-list "-include" flycheck-objc-clang-includes)
            (option-list "-W" flycheck-objc-clang-warnings s-prepend)
            (option-list "-D" flycheck-objc-clang-definitions s-prepend)
            (option-list "-I" flycheck-objc-clang-include-path)
            (option-list "-F" flycheck-objc-clang-framework-path)
            "-x" (eval
                  (cl-case major-mode
                    (objc-mode "objective-c")
                    (c-mode "c")))
            ;; We must stay in the same directory, to properly resolve #include
            ;; with quotes
            source-inplace)
  :error-patterns
  ((info line-start (file-name) ":" line ":" column
            ": note: " (message) line-end)
   (warning line-start (file-name) ":" line ":" column
            ": warning: " (message) line-end)
   (error line-start (file-name) ":" line ":" column
          ": " (or "fatal error" "error") ": " (message) line-end))
  :modes (c-mode objc-mode)
  :next-checkers ((warnings-only . objc-cppcheck)))

(provide 'flycheck-objc)
;;; objc-flycheck.el ends here
