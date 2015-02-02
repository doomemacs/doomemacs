# Emacs for the stubborn vimmer

What we do every night, Pinky...

## Installation

```sh
brew install emacs --cocoa --with-imagemagick --with-gnutls`
brew install cask
git clone --recursive https://github.com/hlissner/emacs.d ~/.emacs.d
cd ~/.emacs.d && cask install
```

## Modules

See `./Cask` for what packages I use.

## Customizations

### Ex commands

```
a                      # switch to other file (headers, css/scss, js/coffee, etc.)
ag <search term>       # helm ag search
ag! <search term>      # regex helm ag search
ag[cw]d                # helm ag search in current directory
ag[cw]d!               # regex helm ag search in current directory
al[ign] <regex>        # align selected text that matches <regexp>
cd <dir>               # self-explanitory
en[ew] <file>          # create and open new file template
en[ew]! <file>         # same, but does it silently (ignore editable fields)
er[rors]               # open flycheck errors panel
full[scr]              # toggle emacs fullscreen mode (classic fullscreen)
gdiff                  # (git) show diff on selected hunks
grevert                # (git) revent selected hunks
gstage                 # (git) stage selected hunks
ini                    # run find-file in ~/.emacs.d/
k[ill]                 # kill-this-buffer
k[ill]all              # kill all buffers and windows
k[ill]all!             # kill all project buffers and windows
k[ill]buried           # kill all buried buffers
k[ill]buried!          # kill all buried project buffers
k[ill]o                # clean up buried buffers and unused processes
killpersp              # kill perspective
msg                    # show emacs messages
n[otes]                # run find-file in org-directory
o[rg]agenda            # (org-mode) open org-agenda
o[rg]align             # (org-mode) align all tags
o[rg]archive           # (org-mode) archive item at point
o[rg]attach            # (org-mode) attach file
o[rg]link              # (org-mode) create link
o[rg]refile            # (org-mode) refile at point
o[rg]rgarchive         # (org-mode) archive item at point
o[rg]todo              # (org-mode)
pres[ent]              # toggle presentation mode (bigger fonts)
proj[ect]              # helm-projectile-switch-project
re[gex]                # open re-builder for writing regex
rec[ent]               # helm-recentf
recompile              # byte-compile emacs directory
ref[actor]             # activate emr menu
ren[ame] <new name>    # rename current file to <new name>
retab                  # converts to tabs (or spaces, depending on settings)
snip[pets]             # edit/expand one of this major-mode's snippets
sq[uint]               # narrow to selected region
sw[oop]                # live project-wide ag search
tcd <dir>              # send 'cd <dir>' to tmux (or 'cd (default-directory)' if blank)
```
