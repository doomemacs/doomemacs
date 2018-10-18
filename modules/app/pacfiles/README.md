# pacfiles-mode

Emacs major mode to manage `.pacnew` and `.pacsave` files left by Arch's pacman.
To merge files, *pacfiles-mode* automatically creates an Ediff merge session
that a user can interact with. After finishing the Ediff merge session,
*pacfiles-mode* cleans up the mess that Ediff leaves behind. *pacfiles-mode*
also takes care of keeping the correct permissions of merged files, and
requests passwords (with TRAMP) to act as root when needed.

## Overview
Start *pacfiles-mode* with the command `pacfiles`. Choose a file to merge by
clicking (or pressing `RET`) on `[merge]`. An Ediff session will start; do not
change the name of the Ediff buffers. After finishing the merging process, save
the merged file (e.g., in Ediff's command buffer, use keybindings `w c`) without
changing its name or location and quit Ediff. To apply the merge file to the
file system, click (or press `RET`) on `[apply]`. Quit *pacfiles-mode* by
pressing `q`.

## Main interface

![pacfiles-mode main interface](https://github.com/UndeadKernel/pacfiles-mode/blob/screenshots/main_ui.png "Main user interface")

*pacfiles-mode* searches and lists all `.pacnew` and `.pacsave` update files
found in `/etc` (by default). These update files can be discarded or, with the
help of [Ediff](https://www.gnu.org/software/emacs/manual/html_node/ediff/
"Ediff's manual"), compared or merged. Ediff is automatically setup for the
user; permissions are also taken care for.

### Pending files
`.pacnew` or `.pacsave` file that have not been merged
by the user are shown under the **pending** header. Three actions are
available on these files

![Pending Actions](https://github.com/UndeadKernel/pacfiles-mode/blob/screenshots/pending_actions.png "Pending Actions")
* `[merge]`: start an Ediff session to merge the update
* `[diff]`: compare the update with its base file
* `[delete]`: delete the update and keep the base file as is

If the `[diff]` button is not available, there is no base file to compare with.
In this case, `[merge]` will treat the update, as is, as the file to merge.

### Merged files
`.pacnew` and `.pacsave` files that have been merged, but not applied into the
file system, are shown under the **merged** header. Three actions are
available on these files:

![Merged Actions](https://github.com/UndeadKernel/pacfiles-mode/blob/screenshots/merge_actions.png "Merged Actions")
* `[apply]`: copy the merged file into the file system and delete the update
  file
* `[view]`: show the merge of the update and its base file
* `[discard]`: delete the merge file, keep the update file and the base file
  intact

## Functions and Variables of *pacfiles-mode*

### Commands
* `pacfiles` or `pacfiles-start`: start *pacfiles-mode*.
* `pacfiles-quit`: quit *pacfiles-mode*.
* `pacfiles-revert-buffer`: reload the list of update files

### Configuration variables
* `pacfiles-updates-search-command`: command used to search for `.pacnew` and
  `.pasave` files.
* `pacfiles-activate-no-confirm`: if `t`, do not ask for user input when
  applying or discarding a merged file.
* `pacfiles-merge-file-tmp-location`: location where temporary merge files are
  stored.

### Keybindings
* `n` and `p`: move forward or backwards to the next/previous button.
* `C-c C-n` and `C-c C-p`: move to the next and previous section headers.
* `g` or `r`: refresh the list of files.
* `TAB`: toggle hiding or showing headers

### Evil Specific Keybindings
* `J` and `K`: move forward or backward to the next/previous button.
* `g`: redefined to be `nil` so as to effectively remove the default binding


## Ediff tricks and tips
When merging an update, Ediff's setup will look like this:

![Ediff interface](https://github.com/UndeadKernel/pacfiles-mode/blob/screenshots/ediff_ui.png "Ediff interface")

Ediff's control buffer (on the bottom) will be focused and ready to receive user
commands. The following key bindings are most useful:

* `n` / `p`: go to the next / previous place where there is a difference
* `a`: place in the merge buffer the contents highlighted in the top left window
  (always the update file)
* `b`: place in the merge buffer the contents highlighted in the top right
  window (always the base file to update)
* `w c`: save the contents of the merge buffer
* `q`: quit Ediff
* `?`: show Ediff's help

When saving the merge buffer, do not change the merge buffer's location or name.
You can switch to the merge buffer, buffer **C** in Ediff, and modify it as any
other buffer.
