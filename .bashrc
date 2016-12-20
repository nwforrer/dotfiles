# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# User specific aliases and functions

alias killsql='kill -9 `ps aux | grep sqldeveloper | grep classpath | cut --complement -c1-9 | cut -c1-5`'

alias dots='git --git-dir=$HOME/.dots.git/ --work-tree=$HOME'

export GOROOT=$HOME/usr/local/src/go
export GOPATH=$HOME/src/go

export PATH=$PATH:$GOPATH/bin

# Unlimited bash history
export HISTFILESIZE=-1
export HISTSIZE=-1
