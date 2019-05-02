---
name: "Bug report"
about: "Something went wrong, please fix it!"
title: "[BUG] "
labels: "is:bug", "status:pending-review"
---

**Describe the bug**
Begin with a short description of what the bug is.

If possible, include screen{shots,casts} and a link to your private config, if
it is available publicly.

<details>
<pre>
If you received an error, paste the backtrace here.

To acquire a backtrace either:

1. Turn on debug mode with `M-x toggle-debug-on-error` or
2. Start Emacs with `emacs --debug-init`
3. Or, if the error occurred while using `bin/doom`, use the `-d` or `--debug`
   switch.

Then recreate the error and Emacs should produce a backtrace.
</pre>
</details>

**Steps to reproduce**
1. Select these example steps,
2. Delete them,
3. And replace them with precise steps to reproduce your issue.
4. Fill in "system information" below.

**Expected behavior**
Describe what you expected to happen.

**System information**
Paste the output of `M-x doom/info` or `~/.emacs.d/bin/doom info` here.
