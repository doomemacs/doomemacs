---
name: Bug report
about: Something went wrong, please fix it!
labels: is:bug
title: "[BUG] "
assignees: ''
---

**Describe the issue**
Start with a brief 1 or 2 sentence summary of issue.

Then follow with a longer explanation, if necessary. Here are some suggestions
on what to include:
- What you expected vs what actually happened
- Screenshots/casts of your issue
- A link to your private config
- Labels for any keys you reference (use `SPC h k` to inspect a key)
- Any warnings or errors logged to \*Messages\* (`SPC h e` or `M-x
  view-echo-area-messages`).

<details><pre>
If available, please a backtrace of the error here.

To acquire a backtrace, enable `debug-on-error` then recreate the error. Here
are ways to enable `debug-on-error`:
- `M-x toggle-debug-on-error`,
- Start Emacs with `emacs --debug-init`
- If the error occurred while using `bin/doom`, use the `-d`/`--debug`
- switches or the `DEBUG` environment variable.
</pre></details>


**Steps to reproduce**
1. Select these example steps,
2. Delete them,
3. And replace them with precise steps to reproduce your issue.
4. Fill in "system information" below.


**System information**
<details><pre>
Place the output of `M-x doom/info` or `~/.emacs.d/bin/doom info` here.
</pre></details>
