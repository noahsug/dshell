# scala
#export PATH="/usr/local/bin:/usr/local/scala/scala-2.9.2/bin:$PATH"

# npm installs
#export PATH="/usr/local/share/npm/bin:$PATH"

# bash completion for brew
#if [ -f `brew --prefix`/etc/bash_completion ]; then
#  . `brew --prefix`/etc/bash_completion
#fi

# bash completion for mac ports
if [ -f /opt/local/etc/bash_completion ]; then
  . /opt/local/etc/bash_completion
fi

# fix for "__git_ps1: command not found"
source ~/.bashrc.d/sugarman/.git-prompt.sh

# fix for "completion: function `_git' not found"
source ~/.bashrc.d/sugarman/git-completion.bash

#complete -o bashdefault -o default -o nospace -F _git g 2>/dev/null \
#  || complete -o default -o nospace -F _git g

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

# gcab "adds a button" -> g ca "$ git-branch-name -> adds a button"
function gcab() {
#  git commit -am "\$$(git-branch-name) -> $1"
  gcabi $(git-branch-name) $1
}

function gcabi() {
  #git commit -am "\$$1 -> $2"
  git commit -am "[$1] $2"
}

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

# Update the name of the screen window (set terminal status)
function sts() {
  echo -ne "\ek$1\e\\"
}

function git-branch-name() {
  git branch 2>/dev/null| sed -n '/^\*/s/^\* //p'
}

## emacs stuff
EDITOR="emacs -nw -Q"
VISUAL="emacs -nw -Q"

# alias
alias b="bash"
alias g="git"
alias es="emacsclient -t -a emacs -nw"
alias en="killall emacs ; emacs --daemon"
#alias en="killall emacs ; emacs --daemon ; emacsclient -t -a emacs -nw"
alias e="emacs -nw"
alias eq="emacs -nw -Q"
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias ~="cd ~"
alias la="ls -A"
alias c="clear"
alias di="diff"
alias stsb="sts bash"

### bookmarking
## removed b/c messed with g4 stuff
#function g0() { alias b0="cd $(pwd)" ; export bb0="$(pwd)" ; }
#function g1() { alias b1="cd $(pwd)" ; export bb2="$(pwd)" ; }
#function g2() { alias b2="cd $(pwd)" ; export bb3="$(pwd)" ; }
#function g3() { alias b3="cd $(pwd)" ; export bb4="$(pwd)" ; }
#function g4() { alias b4="cd $(pwd)" ; export bb5="$(pwd)" ; }
#function g5() { alias b5="cd $(pwd)" ; export bb6="$(pwd)" ; }

function bm() {
  export bb$1="$(pwd)"
  alias b$1="cd \$bb$1"
  echo export bb$1=\"$(pwd)\" \; alias b$1=\"cd \$bb$1\" >> ~/.bashrc
}

# search through history on C-P, C-N
bind "C-p":history-search-backward
bind "C-n":history-search-forward

# ack-grep
alias ag="ack-grep"

# edit shortcuts
alias _eb="e ~/.bashrc.d/sugarman/main.sh"
alias _ebp="e ~/.bashrc"
alias _ee="e ~/.emacs.d/sugarman/bindings.el"
alias _eep="e ~/.emacs"
alias _eg="e ~/.gitconfig"
alias _es="e ~/.screenrc"
