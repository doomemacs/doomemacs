#!/bin/bash
#
# On Android (and possibly other Linux distros), /usr/bin/env does not exist.
# This script exists as a workaround for those users.

"$(dirname -- "${BASH_SOURCE:-$0}")/doom" "$@"
