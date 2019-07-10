---
name: "Bug report"
about: "Something went wrong, please fix it!"
title: "[BUG] "
labels: "is:bug", "status:pending-review"
---

**Describe the bug**
Begin with a short description of what the bug is.

Some tips on writing a good bug report:
- Explain what you expected to see and what actually happened
- Include screenshots/casts of your issue, if possible
- Add a link to your private config, if available
- Expand on phrases like "it does not work" and clarify what commands are bound
  on non-default keybinds.
- Check your \*Messages\* buffer for warnings or errors (`SPC h e` or `M-x
  view-echo-area-messages`).
- Include a backtrace of the error, if possible. To acquire a backtrace, first
  you must turn on `debug-on-error` then recreate the error. To do so, either:
  - Turn on debug mode on the fly with `M-x toggle-debug-on-error`,
  - Start Emacs with `emacs --debug-init`
  - Or, if the error occurred while using `bin/doom`, use the `-d` or `--debug`
    switches, or the `DEBUG` environment variable.
- If the backtrace is especially long, put it in
  <details><pre>...</pre></details> tags.


**Steps to reproduce**
1. Select these example steps,
2. Delete them,
3. And replace them with precise steps to reproduce your issue.
4. Fill in "system information" below.


**System information**
<details><pre>
Include the output of `M-x doom/info` or `~/.emacs.d/bin/doom info` here.
</pre></details>
