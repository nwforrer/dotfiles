#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

[[ -x "~/bin/clean-downloads.sh" ]] && sh ~/bin/clean-downloads.sh
[[ -x "~/bin/clean-tmp.sh" ]] && sh ~/bin/clean-tmp.sh

export PATH=$HOME/bin:$HOME/usr/local/bin:$PATH

if [ -d $HOME/usr/local/src/node-v6.9.3-linux-x64/bin ]; then
	export PATH=$HOME/usr/local/src/node-v6.9.3-linux-x64/bin:$PATH
fi

if [ -d $HOME/usr/local/src/todo.txt ]; then
	export PATH=$HOME/usr/local/src/todo.txt:$PATH
fi
