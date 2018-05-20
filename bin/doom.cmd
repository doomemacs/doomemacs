:: Forward the ./doom script to Emacs

@ECHO OFF
PUSHD "%~dp0" >NUL

IF %1=="run" (
   SHIFT
   emacs -Q $* -l init.el -f "doom|run-all-startup-hooks"
) ELSE (
   emacs --quick --script ./doom -- $*
)

POPD >NUL
ECHO ON
