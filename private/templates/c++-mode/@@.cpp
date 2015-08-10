# -*- mode: snippet -*-
# group: file templates
# contributor: Henrik Lissner
# --
/**
 * @file
 * @author  `user-full-name` <`user-mail-address`>
 * @version 0.1
 * @since   0.1 @ `(format-time-string "%Y-%m-%d")`
 */

#include "`(file-name-nondirectory (file-name-sans-extension (buffer-file-name)))`.h"

$0
