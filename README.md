# README #

### Installing dotfiles
Dotfiles are managed using GNU Stow. Install a programs config files by executing `stow <program-name>`.

### Required applications for i3 config ###

* urxvt
* rofi
* polybar
* fontawesome
* tewi
* siji
* i3
* i3lock-color-git
* compton
* feh

## Vim ##

Plugins are configured via Vundle. Set up Vundle:
`$ git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim`

To setup vim-fugitive:
`$ vim -u NONE -c "helptags vim-fugitive/doc" -c q`

### GTK Themes ###

* Arc darker theme
* Firefox arc darker theme

### Font configuration ###

* Generate gtk config files: open lxappearance, make a change, hit apply.
* Manually configure font: set font name to "System San Francisco Display 13" in ~/.gtkrc-2.0 and ~/.config/gtk-3.0/settings.ini

### Icon theme ###

* [Moka](http://samuelhewitt.com/moka/download/moka-icon-theme)

