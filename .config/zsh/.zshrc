# Enable colors and change prompt:
autoload -U colors && colors

autoload -Uz vcs_info

function prompt-length() {
  emulate -L zsh
  local -i COLUMNS=${2:-COLUMNS}
  local -i x y=${#1} m
  if (( y )); then
    while (( ${${(%):-$1%$y(l.1.0)}[-1]} )); do
      x=y
      (( y *= 2 ))
    done
    while (( y > x + 1 )); do
      (( m = x + (y - x) / 2 ))
      (( ${${(%):-$1%$m(l.x.y)}[-1]} = m ))
    done
  fi
  typeset -g REPLY=$x
}

function fill-line() {
  emulate -L zsh
  prompt-length $1
  local -i left_len=REPLY
  prompt-length $2 999999
  local -i right_len=REPLY
  local -i pad_len=$((COLUMNS - left_len - right_len - ${ZLE_RPROMPT_INDENT:-1}))
  if (( pad_len < 1 )); then
    # Not enough space for the right part. Drop it.
    typeset -g REPLY=$1
  else
    local pad=${(pl.$pad_len.. .)}  # pad_len spaces
    typeset -g REPLY=${1}${pad}${2}
  fi
}

function preexec() {
    cmd_start=$(($(print -P %D{%s%6.}) / 1000))
}

function precmd() {
    vcs_info
    if [ $cmd_start ]; then
        local now=$(($(print -P %D{%s%6.}) / 1000))
        local d_ms=$(($now - $cmd_start))
        local d_s=$((d_ms / 1000))
        local ms=$((d_ms % 1000))
        local s=$((d_s % 60))
        local m=$(((d_s / 60) % 60))
        local h=$((d_s / 3600))

        if   ((h > 0)); then cmd_time=${h}h${m}m
            elif ((m > 0)); then cmd_time=${m}m${s}s
            elif ((s > 9)); then cmd_time=${s}.$(printf %03d $ms | cut -c1-2)s # 12.34s
            elif ((s > 0)); then cmd_time=${s}.$(printf %03d $ms)s # 1.234s
            else cmd_time=${ms}ms
        fi

        unset cmd_start
    else
        unset cmd_time
    fi
}

function update_prompt() {
    local REPLY
    fill-line '
%B%{$fg[yellow]%}%n%{$fg[magenta]%}@%{$fg[blue]%}%M   %{$fg[magenta]%}: %~  %{$fg[cyan]%}${vcs_info_msg_0_}' '%(?.%{$fg[green]%} Ok.%{$fg[red]%} %?) %{$fg[yellow]%}󱎫 %{$cmd_time%}'

    PROMPT=$REPLY$'\n'$'%{$fg[red]%}~%{$reset_color%}%b '
}

setopt PROMPT_SUBST
zstyle ':vcs_info:git:*' formats ': %b'

autoload -Uz add-zsh-hook
add-zsh-hook precmd update_prompt




# old config
# PS1='
# %B%{$fg[yellow]%}%n%{$fg[magenta]%}@%{$fg[blue]%}%M   %{$fg[magenta]%}: %~  %{$fg[cyan]%}${vcs_info_msg_0_}
# %{$fg[red]%}~%{$reset_color%}%b '
# RPS1='%?'

# Add to path
export PATH=/home/contramund/.local/bin:$PATH

export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx

export GOBIN="$HOME/.local/bin/go"
export GO=/usr/bin/go

# History in cache directory:
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.cache/zsh/history

# Basic auto/tab complete:
autoload -Uz compinit && compinit -i
zstyle ':completion:*' menu select
zstyle ':completion:*:*:docker:*' option-stacking yes
zstyle ':completion:*:*:docker-*:*' option-stacking yes
zmodload zsh/complist
compinit
_comp_options+=(globdots)		# Include hidden files.

# Differ settings depending of where zsh starts
# if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
#     alias hello='echo "hello from emacs"'

#     # Use vim keys in tab complete menu:
#     bindkey -M menuselect 'h' vi-backward-char
#     bindkey -M menuselect 'k' vi-up-line-or-history
#     bindkey -M menuselect 'l' vi-forward-char
#     bindkey -M menuselect 'j' vi-down-line-or-history
# else
#     alias hello='echo "hello from shell"'

#     # vi mode
#     bindkey -v
#     export KEYTIMEOUT=1

#     # Use ^Space
#     bindkey -M viins '^ ' vi-cmd-mode

#     # Change cursor shape for different vi modes.
#     function zle-keymap-select {
#     if [[ ${KEYMAP} == vicmd ]] ||
#         [[ $1 = 'block' ]]; then
#         echo -ne '\e[1 q'
#     elif [[ ${KEYMAP} == main ]] ||
#         [[ ${KEYMAP} == viins ]] ||
#         [[ ${KEYMAP} = '' ]] ||
#         [[ $1 = 'beam' ]]; then
#         echo -ne '\e[5 q'
#     fi
#     }
#     zle -N zle-keymap-select
#     zle-line-init() {
#         zle -K viins # initiate `vi insert` as keymap (can be removed if `bindkey -V` has been set elsewhere)
#         echo -ne "\e[5 q"
#     }
#     zle -N zle-line-init
#     echo -ne '\e[5 q' # Use beam shape cursor on startup.
#     preexec() { echo -ne '\e[5 q' ;} # Use beam shape cursor for each new prompt

#     # Use vim keys in tab complete menu:
#     bindkey -M menuselect 'h' vi-backward-char
#     bindkey -M menuselect 'k' vi-up-line-or-history
#     bindkey -M menuselect 'l' vi-forward-char
#     bindkey -M menuselect 'j' vi-down-line-or-history
#     bindkey -v '^?' backward-delete-char
# fi



# Use ranger to switch directories and bind it to ctrl-o.
ranger_cd() {
    temp_file="$(mktemp -t "ranger_cd.XXXXXXXXXX")"
    ranger --choosedir="$temp_file" -- "${@:-$PWD}"
    if chosen_dir="$(cat -- "$temp_file")" && [ -n "$chosen_dir" ] && [ "$chosen_dir" != "$PWD" ]; then
        cd -- "$chosen_dir"
    fi
    rm -f -- "$temp_file"
}
bindkey -s '^o' 'ranger_cd\n'

# Make ranger use emacsclient
export EDITOR="emacsclient -r -n "

# Create short commands.
alias v="nvim"
alias pbc="xclip -r -selection clipboard"
alias pbp="xclip -selection clipboard -o"
alias r="ranger"

# Show autosuggestions.
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh 
ZSH_AUTOSUGGEST_STRATEGY=(match_prev_cmd completion)
bindkey '^[^\t' autosuggest-accept
bindkey '^[^M' autosuggest-execute

# to proper vterm work
vterm_printf() {
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

# bindkey '^[p' beginning-of-line
# bindkey '^[a' end-of-line
# bindkey '^[b' backward-word
# bindkey '^[e' forward-word
# bindkey '^[h' backward-char
# bindkey '^[l' forward-char
# bindkey '^[u' history-search-backward
# bindkey '^[k' history-search-forward

# Use vim keys in tab complete menu:
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history

# Load zsh-syntax-highlighting; should be last.
source /usr/share/zsh/plugins/nzsh-syntax-highlighting/zsh-syntax-highlighting.zsh 2>/dev/null
