## -------------------- INCLUDE -------------------- ##
[ -f $HOME/dotfiles/shell/include/environment.shl ] && . $HOME/dotfiles/shell/include/environment.shl
[ -f $HOME/dotfiles/shell/include/aliases.shl ] && . $HOME/dotfiles/shell/include/aliases.shl


## -------------------- INSTANT PROMPT -------------------- ##
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

## -------------------- PLUGINS -------------------- ##
source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source /usr/share/zsh-autosuggestions/zsh-autosuggestions.zsh
#source /usr/share/doc/pkgfile/command-not-found.zsh

zstyle ':completion:*' menu select

## -------------------- THEMES -------------------- ##
ZSH_THEME="powerlevel10k/powerlevel10k"
# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
source $HOME/dotfiles/shell/themes/powerlevel10k/powerlevel10k.zsh-theme


## -------------------- ZSH SETTINGS -------------------- ##
# Completion Progress
COMPLETION_WAITING_DOTS="true"

# History
HISTFILE="$HOME/.zsh_history"
HISTSIZE=100000000
SAVEHIST=10000000
HIST_STAMPS="yyyy/mm/dd"

# Neofetch
neofetch

# Options
autoload compinit && compinit
setopt    AUTO_CD		  # Auto-change directory
setopt    COMPLETE_ALIASES
setopt    EXTENDEDGLOB
setopt    HIST_EXPIRE_DUPS_FIRST  # Remove duplicate before removing uniqe # HISTSIZE > SAVEHIST		
setopt    HIST_FIND_NO_DUPS	  # Do not display duplicates of a previously found line
setopt    HIST_IGNORE_DUPS	  # Don't append if a command already exists
setopt    HIST_IGNORE_ALL_DUPS    # Remove oldest duplicate
setopt    HIST_REDUCE_BLANKS      # Remove useless blanks from commands
setopt    HIST_SAVE_NO_DUPS	  # Older duplicates are omitted while writing history
setopt    HIST_VERIFY		  # History exapnsion line is reloaded into editing buffer ; No direct execution
setopt    INC_APPEND_HISTORY      # Add history lines without exiting shells
setopt    INTERACTIVE_COMMENTS	  # Allow comments in interactive shells
setopt    MAGIC_EQUAL_SUBST	  # Expand after command. For eg. foo
setopt    NULL_GLOB		  # Delete pattern instead of reporting error
setopt    NOTIFY		  # Notify status fo background jobs
