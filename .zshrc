# the following lines were added by compinstall

zstyle ':completion:*' completer _expand _complete _ignored _correct _approximate
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' max-errors 2
zstyle ':completion:*' menu select=2
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl true
zstyle ':completion:*' verbose true
zstyle :compinstall filename '/Users/mars_tran/.zshrc'
# SSHのホスト補完データ
_cache_hosts=(`ruby -ne 'if /^Host\s+(.+)$/; print $1.strip, "\n"; end' ~/.ssh/conf.d/*.conf`)
autoload -Uz compinit
compinit
# End of lines added by compinstall
# Lines configured by zsh-newuser-install
HISTFILE=~/.zsh_history
HISTSIZE=1000000
SAVEHIST=1000000
##コマンド履歴検索
setopt append_history
setopt extended_history
setopt share_history

## zplug setting
source ~/.zplug/init.zsh
zplug "zsh-users/zsh-syntax-highlighting"

##setopt hist_ignore_dups
#setopt magic_equal_subst
setopt auto_cd
setopt auto_pushd
#setopt print_eight_bit
# rootは履歴を残さないようにする
if [ $UID = 0 ]; then
    unset HISTFILE
    SAVEHIST=0
fi

zplug "zsh-users/zsh-syntax-highlighting"
zplug "zsh-users/zsh-history-substring-search"
zplug "zsh-users/zsh-completions"
zplug "zsh-users/zsh-autosuggestions"
zplug "mollifier/anyframe"
zplug "b4b4r07/enhancd", use:init.sh
zplug "b4b4r07/emoji-cli"
zplug "junegunn/fzf", as:command, use:bin/fzf-tmux
zplug "junegunn/fzf-bin", as:command, from:gh-r, rename-to:fzf
zplug "themes/steeef", from:oh-my-zsh #themes
zplug "peco/peco", as:command, from:gh-r
zplug "plugins/git", from:oh-my-zsh, as:plugin #error compdef
zplug "plugins/chucknorris", from:oh-my-zsh, as:plugin
zplug "plugins/common-aliases", from:oh-my-zsh

###zplug "sorin-ionescu/prezto"
if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    else
        echo "Skiped"
    fi
fi

zplug load

function update_git_alias() {
    #git_alias=`alias|grep git|awk -F = '{print $1}'`
    PR_GIT_UPDATE=1
}
add-zsh-hook precmd update_git_alias

## Crtl-Rで履歴検索
function peco-history-selection() {
    BUFFER=`history -n 1 | tail -r  | awk '!a[$0]++' | peco`
    CURSOR=$#BUFFER
    zle reset-prompt
}
zle -N peco-history-selection
bindkey '^R' peco-history-selection
if zplug check "zsh-users/zsh-history-substring-search"; then
    bindkey '^P' history-substring-search-up
    bindkey '^N' history-substring-search-down
fi

## 削除する前に，確認するようにして，誤動作防止
alias rm='rm -i'

## colorize output
alias ls='ls -G'
alias tree='tree -C --dirsfirst'
alias be='bundle exec'
## Show time in right side
RPROMPT='%D-%*'

## Docker
if [ ! "$(docker-machine status dev-machine)" = 'Running' ]; then
    docker-machine start dev-machine && eval $(docker-machine env dev-machine)
    docker images |awk '{if (NR != 1) { gsub("\\.", "", $2); hoge+=('a'); print "docker start " $1 $2} } '|sh
fi
## 色付きlessコマンド
export LESSOPEN="|$HOME/dotfiles/src-hilite-lesspipe.sh %s"
export LESS='-gj10 -RNC'

##alias to mysqlversion 5.7.9
alias mycli57='mycli -uroot -h$(docker-machine ip dev-machine) -P3357 --prompt="\u@\h:\d\n>"'
alias mycli56='mycli -u root -h$(docker-machine ip dev-machine) -P3356 --prompt="\u@\h:\d\n>"'

#Anyenv 設定
export PATH="$HOME/dotfiles/.anyenv/bin:$PATH"
export ANYENV_ROOT="$HOME/dotfiles/.anyenv"
eval "$(anyenv init -)"

#alias to use emacsclient
alias e='myeditor $@'
#alias quick reset emacs server
alias emacsreset='emacsclient -e "(kill-emacs)" && emacs -daemon'

# AWS-CLI
export PATH=~/.local/bin:$PATH

# GIT COLORFUL
export PATH=$PATH:/usr/local/share/git-core/contrib/diff-highlight

# direnv
eval "$(direnv hook zsh)"
