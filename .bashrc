# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi
# Source cargo env
if [ -f ~/.cargo/env ]; then
	. ~/.cargo/env
fi

# Import colorscheme from 'wal'
#(wal -r &)

# User specific aliases and functions

alias killsql='kill -9 `ps aux | grep sqldeveloper | grep classpath | cut --complement -c1-9 | cut -c1-5`'

alias dots='git --git-dir=$HOME/.dots.git/ --work-tree=$HOME'

function cdl() {
	cd "$@" && ls;
}

# Env variables

export GOROOT=$HOME/usr/local/src/go
export GOPATH=$HOME/src/go

export SONAR_SCANNER_HOME=$HOME/usr/local/src/sonar-scanner

export PATH=$PATH:$GOPATH/bin

# Unlimited bash history
export HISTFILESIZE=-1
export HISTSIZE=-1
