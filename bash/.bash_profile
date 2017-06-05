#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

[[ -x "~/bin/clean-downloads.sh" ]] && sh ~/bin/clean-downloads.sh
[[ -x "~/bin/clean-tmp.sh" ]] && sh ~/bin/clean-tmp.sh
