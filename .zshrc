# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# The following lines were added by compinstall

zstyle ':completion:*' completer _expand _complete _ignored _correct _approximate
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' menu select=1
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle :compinstall filename '/home/gavin/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall
# Lines configured by zsh-newuser-install
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt extendedglob
bindkey -e
# End of lines configured by zsh-newuser-install

# if [ "$TERM" = "linux" ]; then
#     echo -en "\e]P0232323" #black
#     echo -en "\e]P82B2B2B" #darkgrey
#     echo -en "\e]P1D75F5F" #darkred
#     echo -en "\e]P9E33636" #red
#     echo -en "\e]P287AF5F" #darkgreen
#     echo -en "\e]PA98E34D" #green
#     echo -en "\e]P3D7AF87" #brown
#     echo -en "\e]PBFFD75F" #yellow
#     echo -en "\e]P48787AF" #darkblue
#     echo -en "\e]PC7373C9" #blue
#     echo -en "\e]P5BD53A5" #darkmagenta
#     echo -en "\e]PDD633B2" #magenta
#     echo -en "\e]P65FAFAF" #darkcyan
#     echo -en "\e]PE44C9C9" #cyan
#     echo -en "\e]P7E5E5E5" #lightgrey
#     echo -en "\e]PFFFFFFF" #white
#     clear #for background artifacting
# fi

setopt hist_ignore_all_dups correct_all

typeset -U PATH path
path=("/home/gavin/bin/" "/home/gavin/.emacs.d/bin" "$path[@]")

#
# variables
#

export LFS="/mnt/lfs"
export SVDIR="~/service"
export YTFZF_EXTMENU=' rofi -dmenu -i -matching fuzzy -width 1500'

#
# alias
#

alias ls="ls --color=auto"
alias grep="grep --color=auto"
alias -g pg="| grep -i"
alias psgrep="ps ax | grep -i"
alias xephyr-awesome="Xephyr :1 & sleep 1 ; DISPLAY=:1 awesome"
alias youtube-dl-audio='youtube-dl -x --output "%(title)s.%(ext)s"' #--audio-format mp3

# if command -v theme.sh > /dev/null; then
#     export THEME_HISTFILE=~/.theme_history
#     [ -e "$THEME_HISTFILE" ] && theme.sh "$(theme.sh -l|tail -n1)"
# fi

#
# vterm
#


## Set typewritten ZSH as a prompt
# export TYPEWRITTEN_SYMBOL="λ"
# export TYPEWRITTEN_CURSOR="terminal"
# export TYPEWRITTEN_COLORS="git_branch:red;current_directory:red;git_rebasing:red"
# fpath+=$HOME/.zsh/typewritten
# autoload -U promptinit; promptinit
# prompt typewritten
source ~/.zsh/powerlevel10k/powerlevel10k.zsh-theme

eval "$(fasd --init posix-alias zsh-hook)"
alias o='a -e xdg-open' # quick opening files with xdg-open

if [[ "$INSIDE_EMACS" = 'vterm' ]] \
    && [[ -n ${EMACS_VTERM_PATH} ]] \
    && [[ -f ${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh ]]; then
    source ${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh
fi

source /usr/share/zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh
bindkey -M emacs '^P' history-substring-search-up
bindkey -M emacs '^N' history-substring-search-down
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
