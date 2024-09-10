# bin/doom.ps1

if (!(Get-Command -Erroraction silentlycontinue emacs.exe)) {
    echo "Couldn't find emacs.exe in your PATH."
    exit 1
}

$doom = "$PSScriptRoot/doom"
$emacs = if ($env:EMACS) { $env:EMACS } else { (Get-Command emacs.exe).Path }
$oldemacsdir = $env:EMACSDIR

try {
    if (-not $env:EMACSDIR)   { $env:EMACSDIR = (get-item $PSScriptRoot).parent.FullName; }
    if (-not $env:__DOOMSH)   { $env:__DOOMSH = "ps1"; }
    if (-not $env:__DOOMPID)  { $env:__DOOMPID = $PID; }
    if (-not $env:__DOOMSTEP) { $env:__DOOMSTEP = 0; }
    if (-not $env:__DOOMGEOM) {
        $cols = (Get-Host).UI.RawUI.WindowSize.Width
        $lines = (Get-Host).UI.RawUI.WindowSize.Height
        $env:__DOOMGEOM = "$cols`x$lines"
    }
    # $env:__DOOMGPIPE = if (-not $env:__DOOMGPIPE) { $env:__DOOMPIPE } else { $env:__DOOMGPIPE }
    # $env:__DOOMPIPE = ""

    & $emacs -q --no-site-file --batch --load "$doom" -- --no-color $args
    if ($LASTEXITCODE -eq 254) {
        & pwsh "$($env:temp)\doom.$($env:__DOOMPID).$($env:__DOOMSTEP).ps1" $PSCommandPath $args
        $exit = $LASTEXITCODE
    }
    exit $exit
} finally {
    # Only clear the env at top-level
    if ($env:__DOOMSTEP -eq 0) {
        $env:EMACSDIR = $oldemacsdir
        Remove-Item Env:\__DOOMSH
        Remove-Item Env:\__DOOMPID
        Remove-Item Env:\__DOOMSTEP
        Remove-Item Env:\__DOOMGEOM
    }
}
