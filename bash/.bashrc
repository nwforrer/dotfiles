#
# bashrc
#

if [ -f /usr/share/git/completion/git-prompt.sh ]; then
	. /usr/share/git/completion/git-prompt.sh
elif [ -f ]; then
	. /usr/share/git-core/contrib/completion/git-prompt.sh
fi

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi
# Source cargo env
if [ -f ~/.cargo/env ]; then
	. ~/.cargo/env
fi

#-----------------------------------------------------------------------
#
#                           ALIASES
#-----------------------------------------------------------------------

alias ls='ls --color=auto'
alias killsql='kill -9 `ps aux | grep sqldeveloper | grep classpath | cut --complement -c1-9 | cut -c1-5`'
#alias mutt='cd ~/Desktop && mutt'
alias events='gcalcli --calendar="nforrer@redhat.com" --detail_length --detail_location agenda 12am 11:59pm'

alias ocopen='oc login https://open.paas.redhat.com:443'
alias ocdev='oc login https://paas.dev.redhat.com:443'
alias ocqa='oc login https://paas.qa.redhat.com:443'
alias ocstage='oc login https://paas.stage.redhat.com:443'
alias ocprod='oc login https://paas.redhat.com:443'

alias t='todo.sh'
complete -o bashdefault -o default -o nospace -F _todo t

#-----------------------------------------------------------------------
#
#                          FUNCTIONS
#-----------------------------------------------------------------------

function cdl() {
	cd "$@" && ls;
}
