#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

[[ -x "~/bin/clean-downloads.sh" ]] && sh ~/bin/clean-downloads.sh
[[ -x "~/bin/clean-tmp.sh" ]] && sh ~/bin/clean-tmp.sh

#-----------------------------------------------------------------------
#
#                           PROMPT
#-----------------------------------------------------------------------

# Syntactic sugar for ANSI escape sequences
txtblk='\e[0;30m' # Black - Regular
txtred='\e[0;31m' # Red
txtgrn='\e[0;32m' # Green
txtylw='\e[0;33m' # Yellow
txtblu='\e[0;34m' # Blue
txtpur='\e[0;35m' # Purple
txtcyn='\e[0;36m' # Cyan
txtwht='\e[0;37m' # White
bldblk='\e[1;30m' # Black - Bold
bldred='\e[1;31m' # Red
bldgrn='\e[1;32m' # Green
bldylw='\e[1;33m' # Yellow
bldblu='\e[1;34m' # Blue
bldpur='\e[1;35m' # Purple
bldcyn='\e[1;36m' # Cyan
bldwht='\e[1;37m' # White
unkblk='\e[4;30m' # Black - Underline
undred='\e[4;31m' # Red
undgrn='\e[4;32m' # Green
undylw='\e[4;33m' # Yellow
undblu='\e[4;34m' # Blue
undpur='\e[4;35m' # Purple
undcyn='\e[4;36m' # Cyan
undwht='\e[4;37m' # White
bakblk='\e[40m'   # Black - Background
bakred='\e[41m'   # Red
badgrn='\e[42m'   # Green
bakylw='\e[43m'   # Yellow
bakblu='\e[44m'   # Blue
bakpur='\e[45m'   # Purple
bakcyn='\e[46m'   # Cyan
bakwht='\e[47m'   # White
txtrst='\e[0m'    # Text Reset

PS1='\[\e[0;31m\]────── \[\e[0;32m\]\W\[\e[0m\] $(__git_ps1 "\[\e[0;33m\]at\[\e[0m\] \[\e[0;34m\]%s\[\e[0m\]") '

#-----------------------------------------------------------------------
#
#                          VARIABLES
#-----------------------------------------------------------------------

# Monitors for polybar config
hostname=$(</etc/hostname)
if [ "$hostname" == "nforrer" ]; then
	  export MONITOR1=DP-2-2
	  export MONITOR2=DP-2-3
elif [ "$hostname" == "archdesk" ]; then
	  export MONITOR1=DVI-I-1
	  export MONITOR2=DVI-D-0
fi

export RANGER_LOAD_DEFAULT_RC=FALSE
export VISUAL=~/bin/edit # uses existing emacs process, or starts new one if not availabel
export EDITOR=~/bin/edit # uses existing emacs process, or starts new one if not availabel

export TODOTXT_DEFAULT_ACTION=ls

# Unlimited bash history
export HISTFILESIZE=-1
export HISTSIZE=-1

# Sonar scanner
export SONAR_SCANNER_HOME=$HOME/usr/local/src/sonar-scanner

export PATH=$HOME/bin:$HOME/usr/local/bin:$PATH

if [ -d $HOME/usr/local/src/node-v6.9.3-linux-x64/bin ]; then
	export PATH=$HOME/usr/local/src/node-v6.9.3-linux-x64/bin:$PATH
fi

if [ -d $HOME/usr/local/src/todo.txt ]; then
	export PATH=$HOME/usr/local/src/todo.txt:$PATH
fi
