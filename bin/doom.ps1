# bin/doom.ps1

if (!(Get-Command -Erroraction silentlycontinue emacs.exe)) {
    echo "Couldn't find emacs.exe in your PATH."
    exit 1
}

$doom = "$PSScriptRoot/doom"
$emacs = if ($env:EMACS) { $env:EMACS } else { (Get-Command emacs.exe).Path }
$emacsargs = "-q", "--no-site-file", "--batch"
$oldemacsdir = $env:EMACSDIR

try {
    $env:EMACSDIR = if (-not $env:EMACSDIR) { (get-item $PSScriptRoot).parent.FullName } else { $env:EMACSDIR }
    $env:__DOOMSH = if (-not $env:__DOOMSH) { "ps1" } else { $env:__DOOMSH }
    $env:__DOOMPID = if (-not $env:__DOOMPID) { $PID } else { $env:__DOOMPID }
    $env:__DOOMSTEP = if (-not $env:__DOOMSTEP) { 0 } else { $env:__DOOMSTEP }
    $cols = (Get-Host).UI.RawUI.WindowSize.Width
    $lines = (Get-Host).UI.RawUI.WindowSize.Height
    $env:__DOOMGEOM = if (-not $env:__DOOMGEOM) { "$cols`x$lines" } else { $env:__DOOMGEOM }
    # $env:__DOOMGPIPE = if (-not $env:__DOOMGPIPE) { $env:__DOOMPIPE } else { $env:__DOOMGPIPE }
    # $env:__DOOMPIPE = ""

    & $emacs $emacsargs --load "$doom" -- --no-color $args
    $exit = $LASTEXITCODE
} finally {
    $env:EMACSDIR = $oldemacsdir
    Remove-Item Env:\__DOOMSH
    Remove-Item Env:\__DOOMPID
    Remove-Item Env:\__DOOMSTEP
    Remove-Item Env:\__DOOMGEOM
}

if ($exit -eq 254) {
    & pwsh "$env:TMPDIR\doom.$($env:__DOOMPID).$($env:__DOOMSTEP).ps1" $PSCommandPath $args
    $exit = $LASTEXITCODE
}
exit $exit
