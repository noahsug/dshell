# Custom binaries, such as rename.
export PATH="~/.bashrc.d/sugarman/bin:$PATH"

# regex-replace
alias fr="node ~/Dropbox/programming/code/fast-replace/src/cli"

# bash completion for brew
#if [ -f `brew --prefix`/etc/bash_completion ]; then
#  . `brew --prefix`/etc/bash_completion
#fi

# relative path to pwd
function rp() {
  realpath --relative-to=$(pwd) $1
}

# run relative path on clipboard, and save
function rpc() {
  echo $(rp $(pbpaste)) | pbcopy
  echo $(pbpaste)
}

function restart-touchbar() {
  pkill "Touch Bar agent"
  killall "ControlStrip"
}
# touch-bar-sucks
alias tbs="restart-touchbar"

# refresh codesearch cindex
function csr() {
  cd ~
  cindex $1 $2 $3 $4 $5 $6 $7 $8 $9
  cd -
}

# list deps: ldeps [folders..]
function ldeps() {
  rg "require\('([^.].*)'\)" $1 -o --no-filename --no-line-number -r "\$1" | sort | uniq
}

# jest list failures
function jlf() {
  pbpaste | rg "FAIL.*(j|t)sx?" | rg "\S+\.(j|t)sx?" -o
}

# diff last two outputs
alias dlo="diff <(!-2 2>&1) <(!! 2>&1)"

# list locally linked node modules
alias nll="ls -l node_modules | grep ^l"

# list ALL linked node modules
alias nllg="npm ls -g --depth=0 --link=true"

# fuzzy match airbnb/frontend directories
function cf() {
  DIR=$(fuzzy-dir.js $@ --dir ~/repos/airbnb --dir ~/repos/airbnb/frontend/) && cd $DIR
}

# find common occurrences between two rg runs
function rgc() {
  comm -12 <(rg "$1" -l | sort) <(rg "$2" -l | sort)
  echo "$(rg "$1" -l | wc -l) $1"
  echo "$(rg "$2" -l | wc -l) $2"
}

# find occurrences in rg run 1 that isn't in rg run 2
function rgd() {
  comm -23 <(rg "$1" -l | sort) <(rg "$2" -l | sort)
  echo "$(rg "$1" -l | wc -l) $1"
  echo "$(rg "$2" -l | wc -l) $2"
}

# find common occurrences between two rgi runs
function rgic() {
  comm -12 <(rgi "$1" "$2" -l | sort) <(rgi "$3" "$4" -l | sort)
  echo "$(rgi "$1" "$2" -l | wc -l) $2"
  echo "$(rgi "$3" "$4" -l | wc -l) $4"
}

# find imports:
function rgi() {
  if [[ -z "$2" ]]; then
    rg "('|\./|/javascripts/)$1[/'.]" -g *.js -g *.jsx -g *.ts -g *.tsx
  elif [[ "$2" == -* ]]; then
    rg "('|\./|/javascripts/)$1[/'.]" "$2" -g *.js -g *.jsx -g *.ts -g *.tsx
  elif [[ "$3" == -* ]]; then
    rg "([/']$1|\.)/$2[/'.]" "$3" -g *.js -g *.jsx -g *.ts -g *.tsx
  else
    rg "([/']$1|\.)/$2[/'.]" -g *.js -g *.jsx -g *.ts -g *.tsx
  fi
}

# find imports + search .eslint:
function rgie() {
  if [[ -z "$2" ]]; then
    rg "('|\./|/javascripts/)$1[/'.]" -g *.js -g *.jsx -g *.ts -g *.tsx
  elif [[ "$2" == -* ]]; then
    rg "('|\./|/javascripts/)$1[/'.]" "$2" -g *.js -g *.jsx -g *.ts -g *.tsx
  elif [[ "$3" == -* ]]; then
    rg "([/']$1|\.)/$2[/'.]" "$3" -g *.js -g *.jsx -g *.ts -g *.tsx
  else
    rg "([/']$1|\.)/$2[/'.]" -g *.js -g *.jsx -g *.ts -g *.tsx
  fi

  RED='\033[0;35m'
  RESET='\033[0m'
  echo ""
  echo -e "$RED.eslintrc$RESET"

  if [[ -z "$2" ]]; then
    rg "('|\./|/javascripts/)$1[/'.]" .eslintrc
  elif [[ "$2" == -* ]]; then
    rg "('|\./|/javascripts/)$1[/'.]" .eslintrc "$2"
  elif [[ "$3" == -* ]]; then
    rg "([/']$1|\.)/$2[/'.]" .eslintrc "$3"
  else
    rg "([/']$1|\.)/$2[/'.]" .eslintrc
  fi
}

# Save bash history
unset HISTFILESIZE
export HISTSIZE=3000
# bellow lines are included in Git Rerun Tools
# export PROMPT_COMMAND="history -a"
# shopt -s histappend

# bash completion for mac ports
if [ -f /opt/local/etc/bash_completion ]; then
  . /opt/local/etc/bash_completion
fi

source ~/.bashrc.d/sugarman/git.sh
source ~/.bashrc.d/sugarman/npm.sh

# ls colors
export LS_COLORS='di=38;5;108:fi=00:*svn-commit.tmp=31:ln=38;5;116:ex=38;5;186'
export CLICOLOR=1
export LSCOLORS=dxhxcxfxbxegedabagaced


# cd + ls -- WARNING change this to cl when scripts depend on cd having no output
function cl() {
  builtin cd "$1"
  ls -A
}

# npm package version
function nv() {
  rg "\"version\": \"(.*)\"" "node_modules/$1/package.json" -r "\$1" --no-line-number -o ||
      rg "\"$1\": \"(.*)\"" "package.json" -r "\$1"
}

# Set title of the terminal tab
function st() {
  echo -en "\033]2;$1\007"
}

# Update the name of the screen window (set terminal status)
function sts() {
  echo -ne "\ek$1\e\\"
}

# run on each, use: g st | rge 'app/.*jsx' 'git co master %;'
#   see: g st | rg 'app/.*jsx' -o | xargs -I % sh -c 'git co master %;'
function rge() {
  cmd=$(([ -z "$2" ] && echo "echo %;") || echo "$2")
  rg $1 -o | xargs -I % sh -c "$cmd"
}

# xargs command
#    see: xe 'cat ~/tmp' 'g dr %'
function xe() {
  "$1" | xargs -I % sh -c "$2"
}

# rg --files
function rgf() {
  rg -g /**/*$1* --files
}

# alias
alias b="bash"
# emacs open in daemon
function eo() {
  emacsclient "$@" &
}
# emacs quick desktop
function ed() {
  emacs -nw --qd="$1"
}
# start emacs server
alias ec="emacsclient"
alias ekill="ec -e '(kill-emacs)'"
alias es="ek; emacs --daemon; ec"
alias e="emacs -nw --no-desktop"
alias eq="emacs -nw -q -l ~/.emacs.d/sugarman/simple.el"
alias eqq="emacs -nw -Q"
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias ~="cd ~"
alias la="ls -A"
alias c="clear"
alias di="diff"
alias stsb="sts bash"

alias g="git"
alias gsc="g show | grep '\(console.\|window.\)'"
alias gscl="g show | grep '\(console.\|window.\|git\)'"
alias gdc="g show | grep '\(console.\|window.\)'"
alias gdcl="g show | grep '\(console.\|window.\|git\)'"
alias gsd="g show | grep 'Debug\.log'"
alias gsdl="g show | grep '\(Debug\.log\|git\)'"

# lint diff
function ld() {
  git diff-index --name-only HEAD~"$1" | rg '.jsx?$' | xargs npm run eslint --silent
}

# lint diff master
function ldm() {
  git diff-index --name-only $(gmb) | rg '.jsx?$' | xargs npm run eslint --silent
}

# lint:fix diff
function ldf() {
  git diff-index --name-only HEAD~"$1" | rg '.jsx?$' | xargs npm run eslint:fix --silent
}

# lint:fix diff master
function ldmf() {
  git diff-index --name-only $(gmb) | rg '.jsx?$' | xargs npm run eslint:fix --silent
}

# test diff master
function tdm() {
  git diff-index --name-only $(gmb) | rg '.jsx?$' | xargs npm run jest
}

## emacs stuff
EDITOR="e"
VISUAL="e"

### bookmarking
## removed b/c messed with g4 stuff
#function g0() { alias b0="cd $(pwd)" ; export bb0="$(pwd)" ; }
#function g1() { alias b1="cd $(pwd)" ; export bb2="$(pwd)" ; }
#function g2() { alias b2="cd $(pwd)" ; export bb3="$(pwd)" ; }
#function g3() { alias b3="cd $(pwd)" ; export bb4="$(pwd)" ; }
#function g4() { alias b4="cd $(pwd)" ; export bb5="$(pwd)" ; }
#function g5() { alias b5="cd $(pwd)" ; export bb6="$(pwd)" ; }

# $ cd Dropbox/programming/code
# $ bm c
# $ bc   // cd Dropbox/programming/code
# $ echo $bbc  // "Dropbox/programming/code"
function bm() {
  export bb$1="$(pwd)"
  alias b$1="cd \$bb$1"
  echo export bb$1=\"$(pwd)\" \; alias b$1=\"cd \$bb$1\" >> ~/.bashrc
}

# search through history on C-P, C-N
bind "C-p":history-search-backward
bind "C-n":history-search-forward

# play pop sound
alias pop="mpg123 ~/.bashrc.d/sugarman/pop.mp3"

# print out unix timestamp in seconds
alias unix="date +%s"

# edit shortcuts
alias _eb="eq ~/.bashrc.d/sugarman/main.sh"
alias _ebg="eq ~/.bashrc.d/sugarman/git.sh"
alias _ebp="eq ~/.bashrc"
alias _ee="eq ~/.emacs.d/sugarman/bindings.el"
alias _eei="eq ~/.emacs.d/sugarman_init.el"
alias _eep="eq ~/.emacs"
alias _eg="eq ~/.gitconfig"
alias _es="eq ~/.screenrc"

alias _ebf="e ~/.bashrc.d/sugarman/main.sh"
alias _ebgf="e ~/.bashrc.d/sugarman/git.sh"
alias _ebpf="e ~/.bashrc"
alias _eef="e ~/.emacs.d/sugarman/bindings.el"
alias _eeif="e ~/.emacs.d/sugarman_init.el"
alias _eepf="e ~/.emacs"
alias _egf="e ~/.gitconfig"
alias _esf="e ~/.screenrc"

# Find pid running on specific port.
function pport() {
  lsof -i tcp:"$1"
}

# Kill process
function skill() {
  sudo kill -9 $(pgrep $1)
}

# get last modified item
function lsl() {
 ls -ltr | tail -n1 | rg '\S+$' -o
}

# node
alias run="npm run"

# watch src npm run test
function watch() {
  "$2" "$3" "$4"
  fswatch -o "$1" | xargs -n1 "$2" "$3" "$4"
}

# emacs grap - open all files that have code matching regex
function erg() {
  emacs $(rg -l $1)
}

set +H

## complex PS1
# if [ -f ~/.bashrc.d/sugarman/.bashgit ]; then
#     . ~/.bashrc.d/sugarman/.bashgit
# fi

# if [ -f "$(brew --prefix)/opt/bash-git-prompt/share/gitprompt.sh" ]; then
#   __GIT_PROMPT_DIR=$(brew --prefix)/opt/bash-git-prompt/share
#   GIT_PROMPT_ONLY_IN_REPO=1
#   source "$(brew --prefix)/opt/bash-git-prompt/share/gitprompt.sh"
# fi

# RESET="$(tput sgr0)"
#
# DIR_COLOR="$(tput setaf 187)"
# BRANCH_COLOR="$(tput setaf 174)"
#
# git_prompt() {
#   BRANCH=$(git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/')
#
#   if [ ! -z "$BRANCH" ]; then
#     echo -n "$BRANCH_COLOR[$BRANCH"
#
#     DIRTY=$(git diff --shortstat 2> /dev/null | tail -n1)
#     if [ ! -z "$DIRTY" ]; then
#       echo -n " ✗"
#     fi
#     echo "$BRANCH_COLOR]"
#   fi
# }
#
# PS1='\[$DIR_COLOR\w\[$BRANCH_COLOR\]$(git_prompt) \[$RESET\]'

## Slightly complex PS1

# Set color based on clean/staged/dirty.
git_status="$(git status 2> /dev/null)"
if [[ ${git_status} =~ "working directory clean" ]]; then
  state=""
elif [[ ${git_status} =~ "Changes to be committed" ]]; then
  state="${YELLOW}"
else
  state="${LIGHT_RED}"
fi


## simple PS1
. ~/.bashrc.d/sugarman/git-prompt.sh
case $TERM in
  dumb)
    PS1='\W\$ '
    ;;
  *)
    PS1='\[\e[38;5;187m\]\w\[\e[38;5;174m\]$(__git_ps1 "[%s]") \[\e[m\]'
    ;;
esac
