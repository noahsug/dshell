# scala
export PATH="/usr/local/bin:/usr/local/scala/scala-2.9.2/bin:$PATH"

# npm installs
export PATH="/usr/local/share/npm/bin:$PATH"

# bash completion for brew
#if [ -f `brew --prefix`/etc/bash_completion ]; then
#  . `brew --prefix`/etc/bash_completion
#fi

# bash completion for mac ports
if [ -f /opt/local/etc/bash_completion ]; then
  . /opt/local/etc/bash_completion
fi

# fix for "__git_ps1: command not found"
source ~/.bashrc.d/sug/.git-prompt.sh

# fix for "completion: function `_git' not found"
source ~/.bashrc.d/sug/git-completion.bash

complete -o bashdefault -o default -o nospace -F _git g 2>/dev/null \
  || complete -o default -o nospace -F _git g

# ls colors
export LS_COLORS='di=38;5;108:fi=00:*svn-commit.tmp=31:ln=38;5;116:ex=38;5;186'
export CLICOLOR=1
export LSCOLORS=dxhxcxfxbxegedabagaced

# PS1
case $TERM in
  dumb)
    export PS1='\W\$ '
    ;;
  *)
    export PS1='\[\e[38;5;187m\]\w\[\e[38;5;174m\]$(__git_ps1 "[%s]") \[\e[m\]'
    ;;
esac

# cd + ls -- WARNING change this to cl when scripts depend on cd having no output
function cl() {
  builtin cd "$1"
  ls -A
}

# diff deleted (assuming first file is original)
function did() {
  diff "$1" "$2" | grep ">"
}

# diff added (assuming first file is origonal)
function dia() {
  diff "$1" "$2" | grep "<"
}

# Set title of the terminal tab
function st() {
  echo -en "\033]2;$1\007"
}

# Update the name of the screen window
function sts() {
  echo -ne "\ek$1\e\\"
}

function git-branch-name() {
  git branch 2>/dev/null| sed -n '/^\*/s/^\* //p'
}

## emacs stuff
#EDITOR="emacsclient -t"
#VISUAL="emacsclient -t"
EDITOR="emacs -nw"
VISUAL="emacs -nw"

# alias
alias g="git"
#alias e="emacsclient -t -a emacs -nw"
alias e="emacs -nw"
alias es="killall emacs ; emacs --daemon"
alias en="killall emacs ; emacs --daemon ; emacsclient -t -a emacs -nw"
alias ei="emacs -nw"
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias ~="cd ~"
alias la="ls -A"
alias c="clear"
alias di="diff"

# bookmarking
function g0() { alias b0="cd $(pwd)" ; export bb0="$(pwd)" ; }
function g1() { alias b1="cd $(pwd)" ; export bb2="$(pwd)" ; }
function g2() { alias b2="cd $(pwd)" ; export bb3="$(pwd)" ; }
function g3() { alias b3="cd $(pwd)" ; export bb4="$(pwd)" ; }
function g4() { alias b4="cd $(pwd)" ; export bb5="$(pwd)" ; }
function g5() { alias b5="cd $(pwd)" ; export bb6="$(pwd)" ; }

function bm() {
  export bb$1="$(pwd)"
  alias b$1="cd \$bb$1"
  echo export bb$1=\"$(pwd)\" \; alias b$1=\"cd \$bb$1\" >> ~/.bashrc
}

# search through history on C-P, C-N
bind "C-p":history-search-backward
bind "C-n":history-search-forward

# edit shortcuts
alias _eb="e ~/.bashrc.d/sug/main.sh"
alias _ebp="e ~/.bashrc"
alias _ee="e ~/.emacs.d/personal/sug.el"
alias _eep="e ~/.emacs"
alias _eg="e ~/.gitconfig"
alias _es="e ~/.screenrc"
