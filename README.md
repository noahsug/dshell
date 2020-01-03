# DShell
A simple emacs/git/bash environment that I run from Dropbox so all computers have the latest development environment.

## Installation

**Run `make install` then add the following to your .bashrc: `. ~/.bashrc.d/sugarman/main.sh`**

You can add computer specific bash configurations by editing the file "bash/\<hostname\>.sh" where \<hostname\> is the hostname of your computer.

## Mac Setup

#### install homebrew
`/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"`

#### install latest emacs
`brew install emacs`

#### fix ispell
`brew reinstall aspell --with-lang-en`

#### install packages
Run ```M-x load-packages``` in emacs, then exit.

#### install iterm2
https://www.iterm2.com/

#### Cmd as Esc+
1. iterm -> keys -> left option sends Esc+
1. Install https://github.com/frobware/cmd-key-happy
1. cp `example-rcfile.lua` into `cmd-key-happy/` before running `make install-rcfile`

#### Misc
 * Run `ln -s .bashrc .profile` so you don't have to type `bash` every time the terminal starts.
 * iterm -> colors -> color-presets -> import -> base16-harmonic-dark.itermcolors
 * iterm -> keys -> next tab = ^q
 * System Preferences -> Keyboard -> Modifier keys -> Capslock to Control
