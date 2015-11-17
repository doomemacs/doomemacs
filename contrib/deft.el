;;; deft.el --- quickly browse, filter, and edit plain text notes

;;; Copyright (C) 2011-2015 Jason R. Blevins <jrblevin@sdf.org>
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation  and/or other materials provided with the distribution.
;; 3. Neither the names of the copyright holders nor the names of any
;;    contributors may be used to endorse or promote products derived from
;;    this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

;;; Version: 0.6
;;; Author: Jason R. Blevins <jrblevin@sdf.org>
;;; Keywords: plain text, notes, Simplenote, Notational Velocity
;;; URL: http://jblevins.org/projects/deft/

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Deft is an Emacs mode for quickly browsing, filtering, and editing
;; directories of plain text notes, inspired by Notational Velocity.
;; It was designed for increased productivity when writing and taking
;; notes by making it fast and simple to find the right file at the
;; right time and by automating many of the usual tasks such as
;; creating new files and saving files.

;; Deft is open source software and may be freely distributed and
;; modified under the BSD license.  Version 0.6 is the latest stable
;; version, released on June 26, 2015.  You may download it
;; directly here:

;;   * [deft.el](http://jblevins.org/projects/deft/deft.el)

;; To follow or contribute to Deft development, you can either
;; [browse](http://jblevins.org/git/deft.git) or clone the Git
;; repository:

;;     git clone git://jblevins.org/git/deft.git

;; ![Deft 0.6 Screencast](http://jblevins.org/projects/deft/deft-v0.6.gif)

;; The Deft buffer is simply a file browser which lists the titles of
;; all text files in the Deft directory followed by short summaries
;; and last modified times.  The title is taken to be the first line
;; of the file and the summary is extracted from the text that
;; follows.  Files are, by default, sorted in terms of the last
;; modified date, from newest to oldest.

;; All Deft files or notes are simple plain text files where the first
;; line contains a title.  As an example, the following directory
;; structure generated the screenshot above.
;;
;;     % ls ~/.deft
;;     about.txt    browser.txt     directory.txt   operations.txt
;;     ack.txt      completion.txt  extensions.org
;;     binding.txt  creation.txt    filtering.txt
;;
;;     % cat ~/.deft/about.txt
;;     # About
;;
;;     An Emacs mode for slicing and dicing plain text files.

;; Deft's primary operation is searching and filtering.  The list of
;; files can be limited or filtered using a search string, which will
;; match both the title and the body text.  To initiate a filter,
;; simply start typing.  Filtering happens on the fly.  As you type,
;; the file browser is updated to include only files that match the
;; current string.

;; To open the first matching file, simply press `RET`.  If no files
;; match your search string, pressing `RET` will create a new file
;; using the string as the title.  This is a very fast way to start
;; writing new notes.  The filename will be generated automatically.
;; If you prefer to provide a specific filename, use `C-RET` instead.

;; To open files other than the first match, navigate up and down
;; using `C-p` and `C-n` and press `RET` on the file you want to open.
;; When opening a file, Deft searches forward and leaves the point
;; at the end of the first match of the filter string.

;; You can also press `C-o` to open a file in another window, without
;; switching to the other window.  Issue the same command with a prefix
;; argument, `C-u C-o`, to open the file in another window and switch
;; to that window.

;; To edit the filter string, press `DEL` (backspace) to remove the
;; last character or `M-DEL` to remove the last "word".  To yank
;; (paste) the most recently killed (cut or copied) text into the
;; filter string, press `C-y`.  Press `C-c C-c` to clear the filter
;; string and display all files and `C-c C-g` to refresh the file
;; browser using the current filter string.

;; For more advanced editing operations, you can also edit the filter
;; string in the minibuffer by pressing `C-c C-l`.  While in the
;; minibuffer, the history of previous edits can be cycled through by
;; pressing `M-p` and `M-n`.  This form of static, one-time filtering
;; (as opposed to incremental, on-the-fly filtering) may be preferable
;; in some situations, such as over slow connections or on systems
;; where interactive filtering performance is poor.

;; By default, Deft filters files in incremental string search mode,
;; where "search string" will match all files containing both "search"
;; and "string" in any order.  Alternatively, Deft supports direct
;; regexp filtering, where the filter string is interpreted as a
;; formal regular expression.  For example, `^\(foo\|bar\)` matches
;; foo or bar at the beginning of a line.  Pressing `C-c C-t` will
;; toggle between incremental and regexp search modes.  Regexp
;; search mode is indicated by an "R" in the mode line.

;; Common file operations can also be carried out from within Deft.
;; Files can be renamed using `C-c C-r` or deleted using `C-c C-d`.
;; New files can also be created using `C-c C-n` for quick creation or
;; `C-c C-m` for a filename prompt.  You can leave Deft at any time
;; with `C-c C-q`.

;; Unused files can be archived by pressing `C-c C-a`. Files will be
;; moved to `deft-archive-directory', which is a directory named
;; `archive` within your `deft-directory' by default.

;; Files opened with deft are automatically saved after Emacs has been
;; idle for a customizable number of seconds.  This value is a floating
;; point number given by `deft-auto-save-interval' (default: 1.0).

;; Getting Started
;; ---------------

;; To start using it, place it somewhere in your Emacs load-path and
;; add the line

;;     (require 'deft)

;; in your `.emacs` file.  Then run `M-x deft` to start.  It is useful
;; to create a global keybinding for the `deft` function (e.g., a
;; function key) to start it quickly (see below for details).

;; When you first run Deft, it will complain that it cannot find the
;; `~/.deft` directory.  You can either create a symbolic link to
;; another directory where you keep your notes or run `M-x deft-setup`
;; to create the `~/.deft` directory automatically.

;; One useful way to use Deft is to keep a directory of notes in a
;; Dropbox folder.  This can be used with other applications and
;; mobile devices, for example, [nvALT][], [Notational Velocity][], or
;; [Simplenote][] on OS X or [Editorial][], [Byword][], or [1Writer][]
;; on iOS.

;; [nvALT]: http://brettterpstra.com/projects/nvalt/
;; [Notational Velocity]: http://notational.net/
;; [Simplenote]: http://simplenote.com/
;; [Editorial]: https://geo.itunes.apple.com/us/app/editorial/id673907758?mt=8&uo=6&at=11l5Vs&ct=deft
;; [Byword]: https://geo.itunes.apple.com/us/app/byword/id482063361?mt=8&uo=6&at=11l5Vs&ct=deft
;; [1Writer]: https://geo.itunes.apple.com/us/app/1writer-note-taking-writing/id680469088?mt=8&uo=6&at=11l5Vs&ct=deft

;; Basic Customization
;; -------------------

;; You can customize items in the `deft` group to change the default
;; functionality.

;; By default, Deft looks for notes by searching for files with the
;; extensions `.txt`, `.text`, `.md`, `.markdown`, or `.org` in the
;; `~/.deft` directory.  You can customize both the file extension and
;; the Deft directory by running `M-x customize-group` and typing
;; `deft`.  Alternatively, you can configure them in your `.emacs`
;; file:

;;     (setq deft-extensions '("txt" "tex" "org")
;;     (setq deft-directory "~/Dropbox/notes"))

;; The first element of `deft-extensions' (or in Lisp parlance, the
;; car) is the default extension used to create new files.

;; By default, Deft only searches for files in `deft-directory' but
;; not in any subdirectories. Set `deft-recursive' to a non-nil value
;; to enable recursive searching for files in subdirectories:

;;     (setq deft-recursive t)

;; You can easily set up a global keyboard binding for Deft.  For
;; example, to bind it to F8, add the following code to your `.emacs`
;; file:

;;     (global-set-key [f8] 'deft)

;; Reading Files
;; -------------

;; The displayed title of each file is taken to be the first line of
;; the file, with certain characters removed from the beginning. Hash
;; characters, as used in Markdown headers, and asterisks, as in Org
;; Mode headers, are removed.  Additionally, Org mode `#+TITLE:` tags,
;; MultiMarkdown `Title:` tags, LaTeX comment markers (`%`), and
;; Emacs mode-line declarations (e.g., `-*-mode-*-`) are stripped from
;; displayed titles.  This can be customized by changing
;; `deft-strip-title-regexp'.

;; More generally, the title post-processing function itself can be
;; customized by setting `deft-parse-title-function', which accepts
;; the first line of the file as an argument and returns the parsed
;; title to display in the file browser.  The default function is
;; `deft-strip-title', which removes all occurrences of
;; `deft-strip-title-regexp' as described above.

;; For compatibility with other applications which use the filename as
;; the title of a note (rather than the first line of the file), set the
;; `deft-use-filename-as-title' flag to a non-`nil' value. Deft will then
;; use note filenames to generate the displayed titles in the Deft
;; file browser. To enable this, add the following to your `.emacs` file:

;;     (setq deft-use-filename-as-title t)

;; Creating Files
;; --------------

;; Filenames for newly created files are generated by Deft automatically.
;; The process for doing so is determined by the variables
;; `deft-use-filename-as-title' and `deft-use-filter-string-for-filename'
;; as well as the rules in the `deft-file-naming-rules' alist.
;; The possible cases are as follows:

;; 1.  **Default** (`deft-use-filename-as-title' and
;;     `deft-use-filter-string-for-filename' are both `nil'):

;;     The filename will be automatically generated with prefix `deft-`
;;     and a numerical suffix as in `deft-0.ext', `deft-1.ext', ...
;;     The filter string will be inserted as the first line of the file
;;     (which is also used as the display title).

;; 2.  **Filenames as titles** (`deft-use-filename-as-title' is non-`nil'):

;;     When `deft-use-filename-as-title' is non-`nil', the filter string
;;     will be used as the filename for new files (with the appropriate
;;     file extension appended to the end).

;; 3.  **Readable filenames** (`deft-use-filename-as-title' is
;;     `nil' but `deft-use-filter-string-for-filename' is non-`nil'):

;;     In this case you can choose to display the title as parsed from
;;     the first line of the file while also generating readable
;;     filenames for new files based on the filter string. The
;;     variable `deft-use-filter-string-for-filename' controls this
;;     behavior and decouples the title display
;;     (`deft-use-filename-as-title') from the actual filename. New
;;     filenames will be generated from the filter string and
;;     processed according to the rules defined in the
;;     `deft-file-naming-rules' alist. By default, slashes are removed
;;     and replaced by hyphens, but many other options are possible
;;     (camel case, replacing spaces by hyphens, and so on). See the
;;     documentation for `deft-file-naming-rules' for additional
;;     details.

;; Titles inserted into files from the filter string can also be
;; customized for two common modes, `markdown-mode' and `org-mode', by
;; setting the following variables:

;; * `deft-markdown-mode-title-level' - When set to a positive
;;   integer, determines how many hash marks will be added to titles
;;   in new Markdown files. In other words, setting
;;   `deft-markdown-mode-title-level' to `2` will result in new files
;;   being created with level-2 headings of the form `## Title`.

;; * `deft-org-mode-title-prefix' - When non-nil, automatically
;;   generated titles in new `org-mode' files will be prefixed with
;;   `#+TITLE:`.

;; Other Customizations
;; --------------------

;; Deft, by default, lists files from newest to oldest.  You can set
;; `deft-current-sort-method' to 'title to sort by file titles, case
;; ignored.  Or, you can toggle sorting method using
;; `deft-toggle-sort-method'.

;; Incremental string search is the default method of filtering on
;; startup, but you can set `deft-incremental-search' to nil to make
;; regexp search the default.

;; Deft also provides a function for opening files without using the
;; Deft buffer directly.  Calling `deft-find-file' will prompt for a
;; file to open, just like `find-file', but starting from
;; `deft-directory'.  If the file selected is in `deft-directory', it
;; is opened with the usual deft features (automatic saving, automatic
;; updating of the Deft buffer, etc.).  Otherwise, the file will be
;; opened by `find-file' as usual.  Therefore, you can set up a global
;; keybinding for this function to open Deft files anywhere.  For
;; example, to use `C-x C-g`, a neighbor of `C-x C-f`, use the
;; following:

;;     (global-set-key (kbd "C-x C-g") 'deft-find-file)

;; The faces used for highlighting various parts of the screen can
;; also be customized.  By default, these faces inherit their
;; properties from the standard font-lock faces defined by your current
;; color theme.

;; Acknowledgments
;; ---------------

;; Thanks to Konstantinos Efstathiou for writing simplenote.el, from
;; which I borrowed liberally, and to Zachary Schneirov for writing
;; Notational Velocity, whose functionality and spirit I wanted to
;; bring to Emacs.

;; History
;; -------

;; Version 0.6 (2015-06-26):

;; * Recursive search in subdirectories (optional). Set
;;   `deft-recursive' to a non-nil value to enable.
;; * Support for multiple extensions via the `deft-extensions' list.
;;   As such, `deft-extension' is now deprecated.
;; * New variable `deft-create-file-from-filter-string' can enable
;;   generation of new filenames based on the filter string. This decouples
;;   the title display (`deft-use-filename-as-title') from the actual filename
;;   generation.
;; * New variable `deft-file-naming-rules' allows customizing generation
;;   of filenames with regard to letter case and handling of spaces.
;; * New variables `deft-markdown-mode-title-level' and
;;   `deft-org-mode-title-prefix' for automatic insertion of title markup.
;; * Archiving of files in `deft-archive-directory'.
;; * Ability to sort by either title or modification time via
;;   `deft-current-sort-method'.
;; * Update default `deft-strip-title-regexp' to remove the following:
;;     - org-mode `#+TITLE:` tags
;;     - MultiMarkdown `Title:` tags
;;     - LaTeX comment markers (i.e., `%`)
;;     - Emacs mode-line declarations (e.g., `-*-mode-*-`)
;; * Remove leading and trailing whitespace from titles.
;; * Disable visual line mode to prevent lines from wrapping.
;; * Enable line truncation to avoid displaying truncation characters.
;; * Show the old filename as the default prompt when renaming a file.
;; * Call `hack-local-variables' to read file-local variables when
;;   opening files.
;; * Fixed several byte-compilation warnings.
;; * Bug fix: more robust handling of relative and absolute filenames.
;; * Bug fix: use width instead of length of strings for calculations.
;; * Bug fix: fix `string-width' error with empty file.

;; Version 0.5.1 (2013-01-28):

;; * Bug fix: creating files with `C-c C-n` when both the filter string and
;;   `deft-use-filename-as-title' are non-nil resulted in an invalid path.
;; * Bug fix: killed buffers would persist in `deft-auto-save-buffers'.

;; Version 0.5 (2013-01-25):

;; * Implement incremental string search (default) and regexp search.
;;   These search modes can be toggled by pressing `C-c C-t`.
;; * Default search method can be changed by setting `deft-incremental-search'.
;; * Support custom `deft-parse-title-function' for post-processing titles.
;; * The default `deft-parse-title-function' simply strips occurrences of
;;   `deft-strip-title-regexp', which removes Markdown and Org headings.
;; * Open files in another window with `C-o`.  Prefix it with `C-u` to
;;   switch to the other window.
;; * For symbolic links, use modification time of taget for sorting.
;; * When opening files, move point to the end of the first match of
;;   the filter string.
;; * Improved filter editing: delete (`DEL`), delete word (`M-DEL`),
;;   and yank (`C-y`).
;; * Advanced filter editing in minibuffer (`C-c C-l`).

;; Version 0.4 (2011-12-11):

;; * Improved filtering performance.
;; * Optionally take title from filename instead of first line of the
;;   contents (see `deft-use-filename-as-title').
;; * Dynamically resize width to fit the entire window.
;; * Customizable time format (see `deft-time-format').
;; * Handle `deft-directory' properly with or without a trailing slash.

;; Version 0.3 (2011-09-11):

;; * Internationalization: support filtering with multibyte characters.

;; Version 0.2 (2011-08-22):

;; * Match filenames when filtering.
;; * Automatically save opened files (optional).
;; * Address some byte-compilation warnings.

;; Deft was originally written by [Jason Blevins](http://jblevins.org/).
;; The initial version, 0.1, was released on August 6, 2011.

;;; Code:

(require 'cl)
(require 'widget)
(require 'wid-edit)

;; Customization

(defgroup deft nil
  "Emacs Deft mode."
  :group 'local)

(defcustom deft-directory (expand-file-name "~/.deft/")
  "Deft directory."
  :type 'directory
  :safe 'stringp
  :group 'deft)

(defcustom deft-extensions '("txt" "text" "md" "markdown" "org")
  "Any files with these extensions will be listed."
  :type '(repeat string)
  :group 'deft)

(defcustom deft-auto-save-interval 1.0
  "Idle time in seconds before automatically saving buffers opened by Deft.
Set to zero to disable."
  :type 'float
  :group 'deft)

(defcustom deft-time-format " %Y-%m-%d %H:%M"
  "Format string for modification times in the Deft browser.
Set to nil to hide."
  :type '(choice (string :tag "Time format")
                 (const :tag "Hide" nil))
  :group 'deft)

(defcustom deft-use-filename-as-title nil
  "Use filename as title in the *Deft* buffer."
  :type 'boolean
  :group 'deft)

(defcustom deft-use-filter-string-for-filename nil
  "Use the filter string to generate name for the new file."
  :type 'boolean
  :group 'deft)

(defcustom deft-markdown-mode-title-level 0
  "Prefix titles in new Markdown files with required number of hash marks."
  :type 'integer
  :group 'deft)

(defcustom deft-org-mode-title-prefix t
  "Prefix the auto generated title in a new org-mode deft file with #+TITLE:."
  :type 'boolean
  :group 'deft)

(defcustom deft-incremental-search t
  "Use incremental string search when non-nil and regexp search when nil.
During incremental string search, substrings separated by spaces are
treated as subfilters, each of which must match a file.  They need
not be adjacent and may appear in any order.  During regexp search, the
entire filter string is interpreted as a single regular expression."
  :type 'boolean
  :group 'deft)

(defcustom deft-recursive nil
  "Recursively search for files in subdirectories when non-nil."
  :type 'boolean
  :group 'deft)

(defcustom deft-parse-title-function 'deft-strip-title
  "Function for post-processing file titles."
  :type 'function
  :group 'deft)

(defcustom deft-strip-title-regexp
  (concat "\\(?:"
          "^%+" ; line beg with %
          "\\|^#\\+TITLE: *" ; org-mode title
          "\\|^[#* ]+" ; line beg with #, * and/or space
          "\\|-\\*-[[:alpha:]]+-\\*-" ; -*- .. -*- lines
          "\\|^Title:[	 ]*" ; MultiMarkdown metadata
          "\\|#+" ; line with just # chars
          "$\\)")
  "Regular expression to remove from file titles.
Presently, it removes leading LaTeX comment delimiters, leading
and trailing hash marks from Markdown ATX headings, leading
astersisks from Org Mode headings, and Emacs mode lines of the
form -*-mode-*-."
  :type 'regexp
  :safe 'stringp
  :group 'deft)

(defcustom deft-archive-directory "archive/"
  "Deft archive directory.
This may be a relative path from `deft-directory', or an absolute path."
  :type 'directory
  :safe 'stringp
  :group 'deft)

(defcustom deft-file-naming-rules '( (noslash . \"-\") )
  "Alist of cons cells (SYMBOL . VALUE) for `deft-absolute-filename'.

Supported cons car values: `noslash', `nospace', `case-fn'.

Value of `slash' is a string which should replace the forward slash characters
in the file name. The default behavior is to replace slashes with hyphens in the
file name. To change the replacement charcter to an underscore, one could use:

   (setq deft-file-naming-rules '((noslash . \"_\")))

Value of `nospace' is a string which should replace the space characters in the
file name. Below example replaces spaces with underscores in the file names:

   (setq deft-file-naming-rules '((nospace . \"_\")))

Value of `case-fn' is a function name that takes a string as input that has to
be applied on the file name. Below example makes the file name all lower case:

   (setq deft-file-naming-rules '((case-fn . downcase)))

It is also possible to use a combination of the above cons cells to get file
name in various case styles like,

snake_case:

    (setq deft-file-naming-rules '((noslash . \"_\")
                                   (nospace . \"_\")
                                   (case-fn . downcase)))

or CamelCase

    (setq deft-file-naming-rules '((noslash . \"\")
                                   (nospace . \"\")
                                   (case-fn . capitalize)))

or kebab-case

    (setq deft-file-naming-rules '((noslash . \"-\")
                                   (nospace . \"-\")
                                   (case-fn . downcase)))
"
  :group 'deft)

;; Faces

(defgroup deft-faces nil
  "Faces used in Deft mode"
  :group 'deft
  :group 'faces)

(defface deft-header-face
  '((t :inherit font-lock-keyword-face :bold t))
  "Face for Deft header."
  :group 'deft-faces)

(defface deft-filter-string-face
  '((t :inherit font-lock-string-face))
  "Face for Deft filter string."
  :group 'deft-faces)

(defface deft-filter-string-error-face
  '((t :inherit font-lock-warning-face))
  "Face for Deft filter string when regexp is invalid."
  :group 'deft-faces)

(defface deft-title-face
  '((t :inherit font-lock-function-name-face :bold t))
  "Face for Deft file titles."
  :group 'deft-faces)

(defface deft-separator-face
  '((t :inherit font-lock-comment-delimiter-face))
  "Face for Deft separator string."
  :group 'deft-faces)

(defface deft-summary-face
  '((t :inherit font-lock-comment-face))
  "Face for Deft file summary strings."
  :group 'deft-faces)

(defface deft-time-face
  '((t :inherit font-lock-variable-name-face))
  "Face for Deft last modified times."
  :group 'deft-faces)

;; Constants

(defconst deft-version "0.6")

(defconst deft-buffer "*Deft*"
  "Deft buffer name.")

(defconst deft-separator " --- "
  "Text used to separate file titles and summaries.")

(defconst deft-empty-file-title "[Empty file]"
  "Text to use as title for empty files.")

;; Global variables

(defvar deft-mode-hook nil
  "Hook run when entering Deft mode.")

(defvar deft-filter-regexp nil
  "A list of string representing the current filter used by Deft.

In incremental search mode, when `deft-incremental-search' is
non-nil, the elements of this list are the individual words of
the filter string, in reverse order.  That is, the car of the
list is the last word in the filter string.

In regexp search mode, when `deft-incremental-search' is nil,
this list has a single element containing the entire filter
regexp.")

(defvar deft-current-files nil
  "List of files matching current filter.")

(defvar deft-current-sort-method 'mtime
  "Current file soft method.
Available methods are 'mtime and 'title.")

(defvar deft-all-files nil
  "List of all files in `deft-directory'.")

(defvar deft-hash-contents nil
  "Hash containing complete cached file contents, keyed by filename.")

(defvar deft-hash-mtimes nil
  "Hash containing cached file modification times, keyed by filename.")

(defvar deft-hash-titles nil
  "Hash containing cached file titles, keyed by filename.")

(defvar deft-hash-summaries nil
  "Hash containing cached file summaries, keyed by filename.")

(defvar deft-auto-save-buffers nil
  "List of buffers that will be automatically saved.")

(defvar deft-window-width nil
  "Width of Deft buffer.")

(defvar deft-filter-history nil
  "History of interactive filter strings.")

(defvar deft-regexp-error nil
  "Flag for indicating invalid regexp errors.")

(defvar deft-default-extension nil
  "Default file extension of newly created files.")

;; Keymap definition

(defvar deft-mode-map
  (let ((i 0)
        (map (make-keymap)))
    ;; Make multibyte characters extend the filter string.
    (set-char-table-range (nth 1 map) (cons #x100 (max-char))
                          'deft-filter-increment)
    ;; Extend the filter string by default.
    (setq i ?\s)
    (while (< i 256)
      (define-key map (vector i) 'deft-filter-increment)
      (setq i (1+ i)))
    ;; Handle backspace and delete
    (define-key map (kbd "DEL") 'deft-filter-decrement)
    (define-key map (kbd "M-DEL") 'deft-filter-decrement-word)
    ;; Handle return via completion or opening file
    (define-key map (kbd "RET") 'deft-complete)
    ;; Filtering
    (define-key map (kbd "C-c C-l") 'deft-filter)
    (define-key map (kbd "C-c C-c") 'deft-filter-clear)
    (define-key map (kbd "C-y") 'deft-filter-yank)
    ;; File creation
    (define-key map (kbd "C-c C-n") 'deft-new-file)
    (define-key map (kbd "C-c C-m") 'deft-new-file-named)
    (define-key map (kbd "<C-return>") 'deft-new-file-named)
    ;; File management
    (define-key map (kbd "C-c C-d") 'deft-delete-file)
    (define-key map (kbd "C-c C-r") 'deft-rename-file)
    (define-key map (kbd "C-c C-f") 'deft-find-file)
    (define-key map (kbd "C-c C-a") 'deft-archive-file)
    ;; Settings
    (define-key map (kbd "C-c C-t") 'deft-toggle-incremental-search)
    (define-key map (kbd "C-c C-s") 'deft-toggle-sort-method)
    ;; Miscellaneous
    (define-key map (kbd "C-c C-g") 'deft-refresh)
    (define-key map (kbd "C-c C-q") 'quit-window)
    ;; Widgets
    (define-key map [down-mouse-1] 'widget-button-click)
    (define-key map [down-mouse-2] 'widget-button-click)
    (define-key map (kbd "<tab>") 'widget-forward)
    (define-key map (kbd "<backtab>") 'widget-backward)
    (define-key map (kbd "<S-tab>") 'widget-backward)
    (define-key map (kbd "C-o") 'deft-open-file-other-window)
    map)
  "Keymap for Deft mode.")

;; Helpers

(defun deft-whole-filter-regexp ()
  "Join incremental filters into one."
  (mapconcat 'identity (reverse deft-filter-regexp) " "))

(defun deft-search-forward (str)
  "Function to use when matching files against filter strings.
This function calls `search-forward' when `deft-incremental-search'
is non-nil and `re-search-forward' otherwise."
  (if deft-incremental-search
      (search-forward str nil t)
    (re-search-forward str nil t)))

(defun deft-set-mode-name ()
  (if deft-incremental-search
      (setq mode-name "Deft")
    (setq mode-name "Deft/R")))

(defun deft-toggle-incremental-search ()
  "Toggle the `deft-incremental-search' setting."
  (interactive)
  (cond
   (deft-incremental-search
    (setq deft-incremental-search nil)
    (message "Regexp search"))
   (t
    (setq deft-incremental-search t)
    (message "Incremental string search")))
  (deft-filter (deft-whole-filter-regexp) t)
  (deft-set-mode-name))

(defun deft-toggle-sort-method ()
  "Toggle file sorting method defined in `deft-current-sort-method'."
  (interactive)
  (setq deft-current-sort-method
        (if (eq deft-current-sort-method 'mtime) 'title 'mtime))
  (deft-refresh))

(defun deft-filter-regexp-as-regexp ()
  "Return a regular expression corresponding to the current filter string.
When `deft-incremental-search' is non-nil, we must combine each individual
whitespace separated string.  Otherwise, the `car' of `deft-filter-regexp'
is the complete regexp."
  (if deft-incremental-search
      (mapconcat 'regexp-quote (reverse deft-filter-regexp) "\\|")
    (car deft-filter-regexp)))

;; File processing

(defun deft-chomp (str)
  "Trim leading and trailing whitespace from STR."
  (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" str))

(defun deft-base-filename (file)
  "Strip `deft-directory' and `deft-extension' from filename FILE."
  (let* ((deft-dir (file-name-as-directory (expand-file-name deft-directory)))
         (len (length deft-dir))
         (file (substring file len)))
    (file-name-base file)))

(defun deft-find-all-files ()
  "Return a list of all files in the Deft directory.

See `deft-find-files'."
  (deft-find-files deft-directory))

(defun deft-find-files (dir)
  "Return a list of all files in the directory DIR.

It is important to note that the return value is a list of
absolute filenames.  These absolute filenames are used as keys
for the various hash tables used for storing file metadata and
contents.  So, any functions looking up values in these hash
tables should use `expand-file-name' on filenames first.

If `deft-recursive' is non-nil, then search recursively in
subdirectories of `deft-directory' (with the exception of
`deft-archive-directory').

See `deft-find-all-files'."
  (if (file-exists-p dir)
      (let ((archive-dir (expand-file-name (concat deft-directory "/"
                                                   deft-archive-directory "/")))
            (files (directory-files dir t "." t))
            result)
        (dolist (file files)
          (cond
           ;; Recurse into subdirectory if `deft-recursive' is non-nil
           ;; and the directory is not ".", "..", or `deft-archive-directory'.
           ((file-directory-p file)
            (when (and deft-recursive
                       (not (string-match "/\\(\\.\\|\\.\\.\\)$" file))
                       (not (string-prefix-p archive-dir
                                             (expand-file-name (concat file "/")))))
              (setq result (append (deft-find-files file) result))))
           ;; Collect names of readable files ending in `deft-extension'
           ((and (file-readable-p file)
                 (not (backup-file-name-p file))
                 (member (file-name-extension file) deft-extensions))
            (setq result (cons file result)))))
        result)))

(defun deft-strip-title (title)
  "Remove all strings matching `deft-strip-title-regexp' from TITLE."
  (deft-chomp (replace-regexp-in-string deft-strip-title-regexp "" title)))

(defun deft-parse-title (file contents)
  "Parse the given FILE and CONTENTS and determine the title.
If `deft-use-filename-as-title' is `nil', the title is taken to
be the first non-empty line of the FILE. Else the base name of the FILE is
used as title."
  (if deft-use-filename-as-title
      (deft-base-filename file)
    (let ((begin (string-match "^.+$" contents)))
      (if begin
          (funcall deft-parse-title-function
                   (substring contents begin (match-end 0)))))))

(defun deft-parse-summary (contents title)
  "Parse the file CONTENTS, given the TITLE, and extract a summary.
The summary is a string extracted from the contents following the
title."
  (let ((summary (replace-regexp-in-string "[\n\t]" " " contents)))
    (if (and (not deft-use-filename-as-title) title)
        (if (string-match (regexp-quote title) summary)
            (deft-chomp (substring summary (match-end 0) nil))
          "")
      summary)))

(defun deft-cache-file (file)
  "Update file cache if FILE exists."
  (when (file-exists-p file)
    (add-to-list 'deft-all-files file)
    (let ((mtime-cache (deft-file-mtime file))
          (mtime-file (nth 5 (file-attributes (file-truename file)))))
      (if (or (not mtime-cache)
              (time-less-p mtime-cache mtime-file))
          (deft-cache-newer-file file mtime-file)))))

(defun deft-cache-newer-file (file mtime)
  "Update cached information for FILE with given MTIME."
  ;; Modification time
  (puthash file mtime deft-hash-mtimes)
  (let (contents title)
    ;; Contents
    (with-current-buffer (get-buffer-create "*Deft temp*")
      (insert-file-contents file nil nil nil t)
      (setq contents (concat (buffer-string))))
    (puthash file contents deft-hash-contents)
    ;; Title
    (setq title (deft-parse-title file contents))
    (puthash file title deft-hash-titles)
    ;; Summary
    (puthash file (deft-parse-summary contents title) deft-hash-summaries))
  (kill-buffer "*Deft temp*"))

(defun deft-file-newer-p (file1 file2)
  "Return non-nil if FILE1 was modified since FILE2 and nil otherwise."
  (let (time1 time2)
    (setq time1 (deft-file-mtime file1))
    (setq time2 (deft-file-mtime file2))
    (time-less-p time2 time1)))

(defun deft-file-title-lessp (file1 file2)
  "Return non-nil if the title of FILE1 is less than FILE2's in
lexicographic order.
Case is ignored."
  (let ((t1 (deft-file-title file1))
        (t2 (deft-file-title file2)))
    (string-lessp (and t1 (downcase t1))
                  (and t2 (downcase t2)))))

(defun deft-cache-initialize ()
  "Initialize hash tables for caching files."
  (setq deft-hash-contents (make-hash-table :test 'equal))
  (setq deft-hash-mtimes (make-hash-table :test 'equal))
  (setq deft-hash-titles (make-hash-table :test 'equal))
  (setq deft-hash-summaries (make-hash-table :test 'equal)))

(defun deft-cache-update-all ()
  "Update file list and update cached information for each file."
  (setq deft-all-files (deft-find-all-files))             ; List all files
  (mapc 'deft-cache-file deft-all-files)                  ; Cache contents
  (setq deft-all-files (deft-sort-files deft-all-files))) ; Sort by mtime

(defun deft-cache-update-file (file)
  "Update cached information for a single file."
  (deft-cache-file file)                                  ; Cache contents
  (setq deft-all-files (deft-sort-files deft-all-files))) ; Sort by mtime

;; Cache access

(defun deft-file-contents (file)
  "Retrieve complete contents of FILE from cache."
  (gethash file deft-hash-contents))

(defun deft-file-mtime (file)
  "Retrieve modified time of FILE from cache."
  (gethash file deft-hash-mtimes))

(defun deft-file-title (file)
  "Retrieve title of FILE from cache."
  (gethash file deft-hash-titles))

(defun deft-file-summary (file)
  "Retrieve summary of FILE from cache."
  (gethash file deft-hash-summaries))

;; File list display

(defun deft-print-header ()
  "Prints the *Deft* buffer header."
  (if deft-filter-regexp
      (progn
        (widget-insert
         (propertize "Deft: " 'face 'deft-header-face))
        (widget-insert
         (propertize (deft-whole-filter-regexp) 'face
                     (if (and (not deft-incremental-search) deft-regexp-error)
                         'deft-filter-string-error-face
                       'deft-filter-string-face))))
    (widget-insert
         (propertize "Deft" 'face 'deft-header-face)))
  (widget-insert "\n\n"))

(defun deft-buffer-setup ()
  "Render the file browser in the *Deft* buffer."
  (setq deft-window-width (window-width))
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (deft-print-header)

  ;; Print the files list
  (if (not (file-exists-p deft-directory))
      (widget-insert (deft-no-directory-message))
    (if deft-current-files
        (progn
          (mapc 'deft-file-widget deft-current-files))
      (widget-insert (deft-no-files-message))))

  (use-local-map deft-mode-map)
  (widget-setup)
  (goto-char 1)
  (forward-line 2))

(defun deft-string-width (str)
  "A wrapper function for the original string-width which does
not handle nil.  This function return 0 if the given STR is nil,
call the original string-width otherwise"
  (if str
      (string-width str)
    0))

(defun deft-file-widget (file)
  "Add a line to the file browser for the given FILE."
  (when file
    (let* ((key (file-name-nondirectory file))
           (text (deft-file-contents file))
           (title (deft-file-title file))
           (summary (deft-file-summary file))
           (mtime (when deft-time-format
                    (format-time-string deft-time-format (deft-file-mtime file))))
           (mtime-width (deft-string-width mtime))
           (line-width (- deft-window-width mtime-width))
           (title-width (min line-width (deft-string-width title)))
           (summary-width (min (deft-string-width summary)
                               (- line-width
                                  title-width
                                  (length deft-separator)))))
      (widget-create 'link
                     :button-prefix ""
                     :button-suffix ""
                     :button-face 'deft-title-face
                     :format "%[%v%]"
                     :tag file
                     :help-echo "Edit this file"
                     :notify (lambda (widget &rest ignore)
                               (deft-open-file (widget-get widget :tag)))
                     (if title (truncate-string-to-width title title-width)
                       deft-empty-file-title))
      (when (> summary-width 0)
        (widget-insert (propertize deft-separator 'face 'deft-separator-face))
        (widget-insert (propertize (truncate-string-to-width summary summary-width)
                                   'face 'deft-summary-face)))
      (when mtime
        (while (< (current-column) line-width)
          (widget-insert " "))
        (widget-insert (propertize mtime 'face 'deft-time-face)))
      (widget-insert "\n"))))

(add-hook 'window-configuration-change-hook
          (lambda ()
            (when (and (eq (current-buffer) (get-buffer deft-buffer))
                       (not (eq deft-window-width (window-width))))
              (deft-buffer-setup))))

(defun deft-refresh ()
  "Update the file cache, reapply the filter, and refresh the *Deft* buffer."
  (interactive)
  (deft-cache-update-all)
  (deft-refresh-filter))

(defun deft-refresh-filter ()
  "Reapply the filter and refresh the *Deft* buffer.
Call this after any actions which update the cache."
  (interactive)
  (deft-filter-update)
  (deft-refresh-browser))

(defun deft-refresh-browser ()
  "Refresh the *Deft* buffer in the background.
Call this function after any actions which update the filter and file list."
  (when (get-buffer deft-buffer)
    (with-current-buffer deft-buffer
      (deft-buffer-setup))))

(defun deft-no-directory-message ()
  "Return a short message to display when the Deft directory does not exist."
  (concat "Directory " deft-directory " does not exist.\n"))

(defun deft-no-files-message ()
  "Return a short message to display if no files are found."
  (if deft-filter-regexp
      "No files match the current filter string.\n"
    "No files found."))

;; File list file management actions

(defun deft-absolute-filename (slug &optional extension)
  "Return an absolute filename to file named SLUG with optional EXTENSION.
If EXTENSION is not given, `deft-extension' is assumed.

Refer to `deft-file-naming-rules' for setting rules for formatting the file
name."
  (let* ((slug (deft-chomp slug)) ; remove leading/trailing spaces
         (slash-replacement (cdr (assq 'noslash deft-file-naming-rules)))
         (space-replacement (cdr (assq 'nospace deft-file-naming-rules)))
         (case-fn           (cdr (assq 'case-fn deft-file-naming-rules))))
    (when slash-replacement
      (setq slug (replace-regexp-in-string "\/" slash-replacement slug)))
    (when space-replacement
      (setq slug (replace-regexp-in-string " " space-replacement slug)))
    (when case-fn
      (setq slug (funcall case-fn slug)))
    (concat (file-name-as-directory (expand-file-name deft-directory))
            slug "." (or extension deft-default-extension))))

(defun deft-unused-slug ()
  "Return an unused filename slug (short name) in `deft-directory'."
  (let* ((fmt "deft-%d")
         (counter 0)
         (slug (format fmt counter))
         (file (deft-absolute-filename slug)))
    (while (or (file-exists-p file) (get-file-buffer file))
      (setq counter (1+ counter))
      (setq slug (format fmt counter))
      (setq file (deft-absolute-filename slug)))
    slug))

(defun deft-update-visiting-buffers (old new)
  "Rename visited file of buffers visiting file OLD to NEW."
  (let ((buffer (get-file-buffer old)))
    (when buffer
      (with-current-buffer (get-file-buffer old)
        (set-visited-file-name new nil t)
        (hack-local-variables)))))

(defun deft-open-file (file &optional other switch)
  "Open FILE in a new buffer and setting its mode.
When OTHER is non-nil, open the file in another window.  When
OTHER and SWITCH are both non-nil, switch to the other window.
FILE must be a relative or absolute path, with extension."
  (let ((buffer (find-file-noselect file)))
    (with-current-buffer buffer
      (hack-local-variables)
      (when deft-filter-regexp
        (goto-char (point-min))
        (re-search-forward (deft-filter-regexp-as-regexp) nil t))
      ;; Ensure that Deft has been initialized
      (when (not (get-buffer deft-buffer))
        (with-current-buffer (get-buffer-create deft-buffer)
          (deft-mode)))
      ;; Set up auto save hooks
      (add-to-list 'deft-auto-save-buffers buffer)
      (add-hook 'after-save-hook
                (lambda () (save-excursion
                             (deft-cache-update-file buffer-file-name)
                             (deft-refresh-filter)))
                nil t))
    (if other
        (if switch
            (switch-to-buffer-other-window buffer)
          (display-buffer buffer other))
      (switch-to-buffer buffer))))

;;;###autoload
(defun deft-find-file (file)
  "Find FILE interactively using the minibuffer.
FILE must exist and be a relative or absolute path, with extension.
If FILE is not inside `deft-directory', fall back to using `find-file'."
  (interactive
   (list (read-file-name "Deft find file: " deft-directory)))
  (if (and (file-exists-p file)
           (string-match (concat "^" (expand-file-name deft-directory)) file))
      (deft-open-file file)
    (find-file file)))

(defun deft-auto-populate-title-maybe (file)
  "If the filter string is non-nil and `deft-use-filename-as-title' is `nil'
use the filter string to populate the title line in the newly created FILE."
  (when (and deft-filter-regexp (not deft-use-filename-as-title))
    (write-region
     (concat
      (cond
       ((and (> deft-markdown-mode-title-level 0)
             (string-match "^\\(txt\\|text\\|md\\|mdown\\|markdown\\)"
			   deft-default-extension))
        (concat (make-string deft-markdown-mode-title-level ?#) " "))
       ((and deft-org-mode-title-prefix
	     (string-equal deft-default-extension "org"))
	"#+TITLE: "))
      (deft-whole-filter-regexp)
      "\n\n")
     nil file nil)))

(defun deft-new-file-named (slug)
  "Create a new file named SLUG.
SLUG is the short filename, without a path or a file extension. "
  (interactive "sNew filename (without extension): ")
  (let ((file (deft-absolute-filename slug)))
    (if (file-exists-p file)
        (message "Aborting, file already exists: %s" file)
      (deft-auto-populate-title-maybe file)
      (deft-cache-update-file file)
      (deft-refresh-filter)
      (deft-open-file file)
      (with-current-buffer (get-file-buffer file)
        (goto-char (point-max))))))

;;;###autoload
(defun deft-new-file ()
  "Create a new file quickly.
Use either an automatically generated filename or the filter string if non-nil
and `deft-use-filter-string-for-filename' is set.  If the filter string is
non-nil and title is not from filename, use it as the title."
  (interactive)
  (let (slug)
    (if (and deft-filter-regexp deft-use-filter-string-for-filename)
        ;; If the filter string is non-emtpy and titles are taken from
        ;; filenames is set, construct filename from filter string.
        (setq slug (deft-whole-filter-regexp))
      ;; If the filter string is empty, or titles are taken from file
      ;; contents, then use an automatically generated unique filename.
      (setq slug (deft-unused-slug)))
    (deft-new-file-named slug)))

(defun deft-open-file-other-window (&optional arg)
  "When the point is at a widget, open the file in the other window."
  (interactive "P")
  (let ((file (widget-get (widget-at) :tag)))
    (when file
      (deft-open-file file t arg))))

(defun deft-delete-file ()
  "Delete the file represented by the widget at the point.
If the point is not on a file widget, do nothing.  Prompts before
proceeding."
  (interactive)
  (let ((filename (widget-get (widget-at) :tag)))
    (when filename
      (when (y-or-n-p
             (concat "Delete file " (file-name-nondirectory filename) "? "))
        (delete-file filename)
        (delq filename deft-current-files)
        (delq filename deft-all-files)
        (deft-refresh)))))

(defun deft-rename-file ()
  "Rename the file represented by the widget at the point.
If the point is not on a file widget, do nothing."
  (interactive)
  (let ((old-filename (widget-get (widget-at) :tag))
        (deft-dir (file-name-as-directory deft-directory))
        new-filename old-name new-name)
    (when old-filename
      (setq old-name (deft-base-filename old-filename))
      (setq new-name (read-string
                      (concat "Rename " old-name " to (without extension): ")
                      old-name))
      (setq new-filename
            (concat deft-dir new-name "." deft-default-extension))
      (rename-file old-filename new-filename)
      (deft-update-visiting-buffers old-filename new-filename)
      (deft-refresh))))

(defun deft-archive-file ()
  "Archive the file represented by the widget at the point.
If the point is not on a file widget, do nothing."
  (interactive)
  (let (old new name-ext)
    (setq old (widget-get (widget-at) :tag))
    (when old
      (setq name-ext (file-name-nondirectory old))
      (setq new (concat deft-archive-directory name-ext))
      (when (y-or-n-p (concat "Archive file " name-ext "? "))
        ;; if the filename already exists ask for a new name
        (while (file-exists-p new)
          (setq name-ext (read-string "File exists, choose a new name: " name-ext))
          (setq new (concat deft-archive-directory name-ext)))
        (when (not (file-exists-p deft-archive-directory))
          (make-directory deft-archive-directory t))
        (rename-file old new)
        (deft-update-visiting-buffers old new)
        (deft-refresh)))))

;; File list filtering

(defun deft-sort-files-by-mtime (files)
  "Sort FILES in reverse order by modified time."
  (sort files (lambda (f1 f2) (deft-file-newer-p f1 f2))))

(defun deft-sort-files-by-title (files)
  "Sort FILES by title. Case is ignored"
  (sort files (lambda (f1 f2) (deft-file-title-lessp f1 f2))))

(defun deft-sort-files (files)
  "Sort FILES by sort methods selected by
`deft-current-sort-method'."
  (funcall (if (eq deft-current-sort-method 'title)
               'deft-sort-files-by-title
             'deft-sort-files-by-mtime) files))

(defun deft-filter-initialize ()
  "Initialize the filter string (nil) and files list (all files)."
  (interactive)
  (setq deft-filter-regexp nil)
  (setq deft-current-files deft-all-files))

(defun deft-filter-match-file (file &optional batch)
  "Return FILE if FILE matches the current filter regexp."
  (with-temp-buffer
    (insert file)
    (let ((title (deft-file-title file))
          (contents (deft-file-contents file)))
      (when title (insert title))
      (when contents (insert contents)))
    (if batch
	(if (every (lambda (filter)
		     (goto-char (point-min))
                     (deft-search-forward filter))
		   deft-filter-regexp)
	    file)
      (goto-char (point-min))
      (if (deft-search-forward (car deft-filter-regexp))
	  file))))

(defun deft-filter-files (files)
  "Update `deft-current-files' given a list of paths, FILES.
Apply `deft-filter-match-file' to `deft-all-files', handling
any errors that occur."
  (delq nil
        (condition-case nil
            ;; Map `deft-filter-match-file' onto FILES.  Return
            ;; filtered files list and clear error flag if no error.
            (progn
              (setq deft-regexp-error nil)
              (mapcar (lambda (file) (deft-filter-match-file file t)) files))
          ;; Upon an error (`invalid-regexp'), set an error flag
          (error
           (progn
             (setq deft-regexp-error t)
             files)))))

(defun deft-filter-update ()
  "Update the filtered files list using the current filter regexp.
Starts from scratch using `deft-all-files'.  Does not refresh the
Deft buffer."
  (if (not deft-filter-regexp)
      (setq deft-current-files deft-all-files)
    (setq deft-current-files
          (deft-filter-files deft-all-files))))

;; Filters that cause a refresh

(defun deft-filter-clear ()
  "Clear the current filter string and refresh the file browser."
  (interactive)
  (when deft-filter-regexp
    (setq deft-filter-regexp nil)
    (setq deft-current-files deft-all-files)
    (deft-refresh))
  (message "Filter cleared."))

(defun deft-filter (str &optional reset)
  "Update the filter with STR and update the file browser.

In incremental search mode, the car of `deft-filter-regexp' will
be replaced with STR.  If STR has zero length and the length of
the list is greater than one, the empty string will be retained
to simulate whitespace.  However, if STR has zero length and the
list is of length one, then the filter will be cleared.  If STR
is nil, then the car is removed from the list.

In regexp search mode, the current filter string will be replaced
with STR.

When called interactively, or when RESET is non-nil, always
replace the entire filter string."
  (interactive
   (list (read-from-minibuffer "Filter: " (deft-whole-filter-regexp)
                               nil nil 'deft-filter-history)))
  (if deft-incremental-search
      ;; Incremental search mode
      (if (or (called-interactively-p 'any) reset)
          ;; Called interactively or RESET non-nil
          (if (= (length str) 0)
              (setq deft-filter-regexp nil)
            (setq deft-filter-regexp (reverse (split-string str " "))))
        ;; Called noninteractively
        (if (not str)
            ;; If str is nil, remove it and filter with the cdr
            (setq deft-filter-regexp (cdr deft-filter-regexp))
          ;; Use STR it as the new car, even when empty (to simulate
          ;; whitespace), unless this is the only element in the list.
          (if (and (= (length deft-filter-regexp) 1)
                   (= (length str) 0))
              (setq deft-filter-regexp nil)
            (setcar deft-filter-regexp str))))
    ;; Regexp search mode
    (if (> (length str) 0)
        (setq deft-filter-regexp (list str))
      (setq deft-filter-regexp nil)))
  (deft-filter-update)
  (deft-refresh-browser))

(defun deft-filter-increment ()
  "Append character to the filter regexp and update `deft-current-files'."
  (interactive)
  (let ((char last-command-event))
    (if (= char ?\S-\ )
	(setq char ?\s))
    (setq char (char-to-string char))
    (if (and deft-incremental-search (string= char " "))
	(setq deft-filter-regexp (cons "" deft-filter-regexp))
      (progn
	(if (car deft-filter-regexp)
	    (setcar deft-filter-regexp (concat (car deft-filter-regexp) char))
	  (setq deft-filter-regexp (list char)))
	(setq deft-current-files (deft-filter-files deft-current-files))
	(setq deft-current-files (delq nil deft-current-files))
	(deft-refresh-browser)))))

(defun deft-filter-decrement ()
  "Remove last character from the filter, if possible, and update.

In incremental search mode, the elements of `deft-filter-regexp'
are the words of the filter string in reverse order.  In regexp
search mode, the list is a single element containing the entire
filter regexp.  Therefore, in both cases, only the car of
`deft-filter-regexp' is modified."
  (interactive)
  (let ((str (car deft-filter-regexp)))
    (deft-filter
      (if (> (length str) 0)
          ;; If the last string element has at least one character,
          ;; simply remove the last character.
          (substring str 0 -1)
        ;; Otherwise, return nil
        nil))))

(defun deft-filter-decrement-word ()
  "Remove last word from the filter, if possible, and update."
  (interactive)
  (deft-filter
    (if deft-incremental-search
        ;; In incremental search mode, remove the car
        nil
      ;; In regexp search mode, remove last "word" component
      ;(replace-regexp-in-string "[[:space:]\n]*$" "" s)
      (let ((str (car deft-filter-regexp)))
        (if (> (length str) 0)
            (with-temp-buffer
              (insert (concat "\"" str "\""))
              (lisp-interaction-mode)
              (goto-char (- (point-max) 1))
              (backward-word 1)
              (buffer-substring 2 (point)))
          nil)))))

(defun deft-filter-yank ()
  "Append the most recently killed or yanked text to the filter."
  (interactive)
  (deft-filter
    (concat (deft-whole-filter-regexp) (current-kill 0 t)) t))

(defun deft-complete ()
  "Complete the current action.
If there is a widget at the point, press it.  If a filter is
applied and there is at least one match, open the first matching
file.  If there is an active filter but there are no matches,
quick create a new file using the filter string as the title.
Otherwise, quick create a new file."
  (interactive)
  (cond
   ;; Activate widget
   ((widget-at)
    (widget-button-press (point)))
   ;; Active filter string with match
   ((and deft-filter-regexp deft-current-files)
    (deft-open-file (car deft-current-files)))
   ;; Default
   (t
    (deft-new-file))))

;;; Automatic File Saving

(defun deft-auto-save ()
  (save-excursion
    (dolist (buf deft-auto-save-buffers)
      (if (buffer-name buf)
          ;; Save open buffers that have been modified.
          (progn
            (set-buffer buf)
            (when (buffer-modified-p)
              (basic-save-buffer)))
        ;; If a buffer is no longer open, remove it from auto save list.
        (delq buf deft-auto-save-buffers)))))

;;; Mode definition

(defun deft-show-version ()
  "Show the version number in the minibuffer."
  (interactive)
  (message "Deft %s" deft-version))

(defun deft-setup ()
  "Prepare environment by creating the Deft notes directory."
  (interactive)
  (when (not (file-exists-p deft-directory))
    (make-directory deft-directory t))
  (deft-refresh))

;; Deft mode is suitable only for specially-prepared text
(put 'deft-mode 'mode-class 'special)

(defun deft-mode ()
  "Major mode for quickly browsing, filtering, and editing plain text notes.
Turning on `deft-mode' runs the hook `deft-mode-hook'.

\\{deft-mode-map}."
  (message "Deft initializing...")
  (kill-all-local-variables)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq default-directory (expand-file-name deft-directory))

  ;; Visual line mode causes lines to wrap, so turn it off.
  (when (fboundp 'visual-line-mode)
    (visual-line-mode 0))

  ;; Backwards compatibility with deft-extension
  (when (boundp 'deft-extension)
    (setq deft-extensions (cons deft-extension '())))

  (setq deft-default-extension (car deft-extensions))
  (use-local-map deft-mode-map)
  (deft-cache-initialize)
  (deft-cache-update-all)
  (deft-filter-initialize)
  (setq major-mode 'deft-mode)
  (deft-set-mode-name)
  (deft-buffer-setup)
  (when (> deft-auto-save-interval 0)
    (run-with-idle-timer deft-auto-save-interval t 'deft-auto-save))
  (run-mode-hooks 'deft-mode-hook)
  (message "Deft loaded %d files." (length deft-all-files)))

(put 'deft-mode 'mode-class 'special)

;;;###autoload
(defun deft ()
  "Switch to *Deft* buffer and load files."
  (interactive)
  (switch-to-buffer deft-buffer)
  (if (not (eq major-mode 'deft-mode))
      (deft-mode)))

(provide 'deft)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; deft.el ends here
