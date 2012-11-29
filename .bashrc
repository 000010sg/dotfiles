# scm
export EDITOR='vim'

if [ -f $HOME/.bash_aliases ]; then
  source $HOME/.bash_aliases
fi

if [ -f $HOME/.bash_colors ]; then
  source $HOME/.bash_colors
fi

# color the terminal
export CLICOLOR=1

# bash completions
if [ -f $HOME/sandbox/src/leiningen/bash_completion.bash ]; then
  source $HOME/sandbox/src/leiningen/bash_completion.bash
fi

if [ -f $HOME/.ssh_completion ]; then
  source $HOME/.ssh_completion
fi

# git-aware prompt
PS1='\h:\W$(__git_ps1 "(%s)") \u\$ '

# basic path
export PATH="$HOME/bin:/usr/local/bin:/usr/local/sbin:$PATH"

# golang
export GOROOT="/usr/lib/go"
export PATH="$GOROOT/bin:$PATH"
export GOPATH="$HOME/sandbox/gosrc"
export PATH="$HOME/sandbox/gosrc/bin:$PATH"

# fucking fig
export FIG_REMOTE_URL=ftp://devnas/builds/Fig/repos

