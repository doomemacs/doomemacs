:: Forward the ./doom script to Emacs

@ECHO OFF
PUSHD "%~dp0" >NUL

IF "%1"=="run" (
   SHIFT
   start runemacs -Q %* -l ..\init.el -f "doom|run-all-startup-hooks"
) ELSE (
   emacs --quick --script .\doom -- %*
)

POPD >NUL
ECHO ON
