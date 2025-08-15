# bin/doom.ps1
# TODO: Use magic shebang (polyglot)?

function Executable-Find {
    foreach ($exe in $args) {
        if ($exe) {
            $path = Get-Command $exe -ErrorAction SilentlyContinue
            if ($path) { return $path.Path; }
        }
    }
    throw "Could not find in PATH: $($args -join ', ')"
}

$emacs = if ($env:EMACS) { $env:EMACS } else { Executable-Find "emacs.exe" }
$pwsh = Executable-Find "pwsh.exe" "powershell.exe"
$doom = "$PSScriptRoot/doom"
$oldemacsdir = $env:EMACSDIR

try {
    if (-not $env:EMACSDIR)   { $env:EMACSDIR = (get-item $PSScriptRoot).parent.FullName; }
    if (-not $env:__DOOMSH)   { $env:__DOOMSH = "ps1"; }
    if (-not $env:__DOOMPID)  { $env:__DOOMPID = $PID; }
    if (-not $env:__DOOMSTEP) { $env:__DOOMSTEP = 0; }
    if (-not $env:__DOOMGEOM) {
        $cols = (Get-Host).UI.RawUI.WindowSize.Width
        $lines = (Get-Host).UI.RawUI.WindowSize.Height
        $env:__DOOMGEOM = "$($cols)x$($lines)"
    }
    # $env:__DOOMGPIPE = if (-not $env:__DOOMGPIPE) { $env:__DOOMPIPE } else { $env:__DOOMGPIPE }
    # $env:__DOOMPIPE = ""

    & $emacs -q --no-site-file --batch --load "$doom" -- --no-color $args
    if ($LASTEXITCODE -eq 254) {
        # TODO: Use Invoke-Command instead?
        & $pwsh "$($env:temp)\doom.$($env:__DOOMPID).$($env:__DOOMSTEP).ps1" $PSCommandPath $args
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
