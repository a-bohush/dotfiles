# Enable prompt substitution
setopt PROMPT_SUBST

# Set debian_chroot for similar behavior to Ubuntu's default prompt
if [[ -z "$debian_chroot" && -r /etc/debian_chroot ]]; then
    debian_chroot=$(< /etc/debian_chroot)
fi

# Define the prompt
PROMPT='%B%F{green}${debian_chroot:+($debian_chroot)}%n@%m%f:%F{blue}%~%f%b> '

# Prompt explanation:
# - %B ... %b     : Start and end bold formatting
# - %F{color} ... %f : Start and end color
# - ${debian_chroot:+($debian_chroot)} : Show chroot if defined, similar to bash's prompt on Ubuntu
# - %n            : Username
# - %m            : Hostname
# - %~            : Abbreviated working directory (with ~ for home)

# Color correction to ensure colors reset correctly
autoload -Uz colors && colors

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

export EDITOR="/snap/bin/emacs"
export BUNDLER_EDITOR="/snap/bin/emacs"

alias gs='git status'
alias gb='git branch'
alias gd='git diff'
alias gl='git log'

alias ls='ls --color=auto'
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

# Keybindings
bindkey -e
bindkey '^p' history-search-backward
bindkey '^n' history-search-forward
# bindkey '^[w' kill-region

# History
HISTSIZE=10000
HISTFILE=~/.zsh_history
SAVEHIST=$HISTSIZE
HISTDUP=erase
setopt appendhistory
setopt sharehistory
setopt hist_ignore_space
setopt hist_ignore_all_dups
setopt hist_save_no_dups
setopt hist_ignore_dups
setopt hist_find_no_dups


# Set the directory we want to store zinit and plugins
ZINIT_HOME="${XDG_DATA_HOME:-${HOME}/.local/share}/zinit/zinit.git"

# Download Zinit, if it's not there yet
if [ ! -d "$ZINIT_HOME" ]; then
   mkdir -p "$(dirname $ZINIT_HOME)"
   git clone https://github.com/zdharma-continuum/zinit.git "$ZINIT_HOME"
fi

# Source/Load zinit
source "${ZINIT_HOME}/zinit.zsh"

zinit light zsh-users/zsh-autosuggestions
zinit light Aloxaf/fzf-tab # Requires fzf installed


zstyle ':fzf-tab:complete:cd:*' fzf-preview 'ls --color $realpath'
zstyle ':fzf-tab:complete:__zoxide_z:*' fzf-preview 'ls --color $realpath' # Requires zoxide installed

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# Shell integrations
source <(fzf --zsh)
eval "$(zoxide init --cmd cd zsh)"
