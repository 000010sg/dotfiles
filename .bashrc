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

GIT_PS1_SHOWDIRTYSTATE=true
if [ -f /usr/local/etc/bash_completion.d/git-completion.bash ]; then
  source /usr/local/etc/bash_completion.d/git-completion.bash;
  export PS1='\[\033[01;34m\]\w\[\033[00m\]\[\033[01;32m\]$(__git_ps1 " (%s)")\[\033[00m\] \n$ '
fi

# basic path
export PATH="$HOME/bin:/usr/local/bin:/usr/local/sbin:/usr/local/share/python:$PATH"

# clojurescript
export CLOJURESCRIPT_HOME="$HOME/sandbox/src/clojurescript"
export CLOJURESCRIPT_WATCH_HOME="$HOME/sandbox/src/cljs-watch"
export PATH="$CLOJURESCRIPT_HOME/bin:$PATH"
export PATH="$CLOJURESCRIPT_WATCH_HOME:$PATH"

# lbm (ugh)
export LD_LIBRARY_PATH="/site/apps/LBM_3.6/Linux-2.6-glibc-2.5-x86_64/lib"

# golang
export GOROOT="/usr/lib/go"
export PATH="$GOROOT/bin:$PATH"
export GOPATH="$HOME/sandbox/gosrc"

# rbenv 9_9
export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"

# sqldeveloper
export PATH="/site/apps/sqldeveloper:$PATH"

# fucking fig
export FIG_REMOTE_URL=ftp://devnas/builds/Fig/repos

# keep launcher password in file on machine only
if [ -f $HOME/.launcherrc ]; then
  source $HOME/.launcherrc 
fi
