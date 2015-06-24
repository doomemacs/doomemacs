# -*- mode: snippet -*-
# name: default.js file template
# condition: (memq major-mode '(js-mode js2-mode))
# --
/*global Lib*/

${1:include("shared/${2:lib.js}");

}// This function is called when the user runs the action without any argument,
// e.g. by selecting it and hitting enter, space or the right arrow key
// (assuming the action does not specify that it requires an argument in its
// Info.plist). run is also called as a fallback if the more specific runWith...
// function for a given argument type (see below) is not implemented.
function run() {
    $0
}

// This function is called after text input or when the user sends text to the
// action using Send To.
function runWithString(string) {

}

// This function is called when a URL item is sent to the action. This is also
// the case when a Action URL is called.
function runWithURL(url, details) {

}

// This function is called when the user sends a result item of a previous
// action run to the action using Send To.
function runWithItem(item) {

}

// This function is called after showing a file open dialog or when the user
// sends one or more files or folders to the action using Send To.
function runWithPaths(paths) {

}
