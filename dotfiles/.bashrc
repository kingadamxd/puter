export _JAVA_AWT_WM_NONREPARENTING=1

export XDG_CONFIG_HOME=${HOME}/.config
export XDG_CACHE_HOME=${HOME}/.cache
export XDG_DATA_HOME=${HOME}/.local/share
export XDG_STATE_HOME=${HOME}/.local/state

export PATH=${PATH}:~/bin
export PATH=${PATH}:~/puter/scriptz
export PATH=${PATH}:~/.cargo/bin
export LSP_USE_PLISTS=true
export EDITOR="emacsclient -c -a emacs"
export VISUAL="emacsclient -c -a emacs"
export TERMINAL="alacritty"
export BROWSER="firefox"
export DOTNET_CLI_TELEMETRY_OPTOUT=1
export LESSHISTFILE=".history"

export GOPATH=$HOME/go
export PATH=$PATH:/usr/local/go/bin:$GOPATH/bin

export MPD_HOST="localhost"
export MPD_PORT="6669"

alias cls="tput reset"
alias ls='ls --color=auto'
alias ll="ls -lah"
alias grep='grep --color=auto'
alias tmux="tmux -2"
