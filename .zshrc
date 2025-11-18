# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=100000
SAVEHIST=100000
unsetopt beep
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/owen/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

export PATH=$HOME/.local/bin:$PATH
export PATH=/opt/clion-2025.2.4/bin:$PATH
export PATH=/opt/idea-IU-252.27397.103/bin:$PATH



bindkey '^[[1;5C' forward-word
bindkey '^[[1;5D' backward-word

autoload -U promptinit; promptinit
prompt pure

alias ls=lsd

source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh

eval "$(zoxide init zsh)"
