#!/usr/bin/env bash

source VARS

#
echo "Setting up C# (omnisharp)"

git-repo "https://github.com/OmniSharp/omnisharp-server" omnisharp
cd omnisharp && xbuild
mv omnisharp/bin/Debug/OmniSharp.exe ./OmniSharp.exe
rm -rf omnisharp
