#
# ~/.bash_profile
#
# [[ -f ~/.bashrc ]] && . ~/.bashrc
export SHELL=/bin/zsh
[ -z "$ZSH_VERSION" ] && exec /bin/zsh -l

export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx

if [ -e /home/contramund/.nix-profile/etc/profile.d/nix.sh ]; then . /home/contramund/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer


