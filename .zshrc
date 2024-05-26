# -------------------- INCLUDE -------------------- ##
[ -f $HOME/dotfiles/environment.shl ] && . $HOME/dotfiles/environment.shl
[ -f $HOME/dotfiles/aliases.shl ] && . $HOME/dotfiles/aliases.shl


## -------------------- PLUGINS -------------------- ##
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source /usr/share/zsh/plugins/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh
source /usr/share/zsh/plugins/zsh-autocomplete/zsh-autocomplete.plugin.zsh
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
source /usr/share/doc/pkgfile/command-not-found.zsh

zstyle ':completion:*' menu select

## -------------------- THEMES -------------------- ##
#ZSH_THEME="powerlevel10k/powerlevel10k"
eval "$(starship init zsh)"
## -------------------- ZSH SETTINGS -------------------- ##
