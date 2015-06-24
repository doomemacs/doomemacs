# -*- mode: snippet -*-
# group: file templates
# contributor: Henrik Lissner
# --
/**
 * @file
 * @author  `user-full-name` <`user-mail-address`>
 * @version ${1:0.1}
 * @since   $1 @ `(format-time-string "%Y-%m-%d")`
 *
 * @section DESCRIPTION
 *
 * ${2:Some description of this file}
 */

#include <Windows.h>

int CALLBACK WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow)
{
    $0

    return 0;
}
