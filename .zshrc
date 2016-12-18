source ~/.zplug/init.zsh
#setopt correct

#setopt hist_ignore_dups
setopt magic_equal_subst
setopt auto_cd
setopt print_eight_bit
#setopt auto_push
# rootは履歴を残さないようにする

if [ $UID = 0 ]; then
    unset HISTFILE
    SAVEHIST=0
fi

zplug "zsh-users/zsh-syntax-highlighting"
zplug "zsh-users/zsh-history-substring-search"
zplug "zsh-users/zsh-completions"
zplug "mollifier/anyframe"
zplug "b4b4r07/enhancd", use:init.sh
zplug "junegunn/fzf", as:command, use:bin/fzf-tmux
zplug "junegunn/fzf-bin", as:command, from:gh-r, rename-to:fzf
zplug "themes/steeef", from:oh-my-zsh #themes
zplug "peco/peco", as:command, from:gh-r
zplug "plugins/git", from:oh-my-zsh, as:plugin #error compdef
zplug "plugins/chucknorris", from:oh-my-zsh, as:plugin
zplug "plugins/common-aliases", from:oh-my-zsh

##zplug "jeremyFreeAgent/oh-my-zsh-powerline-theme"
##zplug "bhilburn/powerlevel9k", use:powerlevel9k.zsh-theme
##zplug "jimeh/zsh-peco-history"
##zplug "plugins/brew", from:oh-my-zsh
##zplug "themes/duellj", from:oh-my-zsh #themes
##zplug "zakaziko99/agnosterzak-ohmyzsh-theme"
#
##zplug "sorin-ionescu/prezto"
#

if zplug check "zsh-users/zsh-history-substring-search"; then
    bindkey '^P' history-substring-search-up
    bindkey '^N' history-substring-search-down
fi

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
function peco-history-selection() {
    BUFFER=`history -n 1 | tail -r  | awk '!a[$0]++' | peco`
    CURSOR=$#BUFFER
    zle reset-prompt
}
zle -N peco-history-selection
bindkey '^R' peco-history-selection

#補完機能
#fpath=(/usr/local/Cellar/zsh-completions/0.17.0 $fpath)
#fpath=(/usr/local/share/zsh-completions $fpath)
#コマンド履歴検索
export HISTFILE=$HOME"/.zhistory"
export HISTSIZE=50000
export SAVEHIST=4000
setopt append_history
setopt extended_history
setopt share_history

#削除する前に，確認するようにして，誤動作防止
alias rm='rm -i'
#colorize output
alias ls='ls -G'
alias tree='tree -C --dirsfirst'
alias tree1='tree -L 1'
#tmuxinator -> mux
alias mux='tmuxinator'
#alias to use emacsclient
alias e='myeditor $@'
#alias to mysqlversion 5.7.9
alias mycli579='mycli -u root -P 3307 --prompt="\u@\h:\d\n>"'
alias mycli57='mycli -uroot -h$(docker-machine ip dev) -P3357 --prompt="\u@\h:\d\n>"'
#alias quick reset emacs server
alias emacsreset='emacsclient -e "(kill-emacs)" && emacs -daemon'
#anyenvのパスをシステムパスに追加と初期化
export PATH="$HOME/.anyenv/bin:$PATH"
eval "$(anyenv init -)"
#homebrew用のトークン
export HOMEBREW_GITHUB_API_TOKEN="3082b9adf5111add8a69ce6faa682900551c6c61"

export PATH="/usr/local/opt/openssl/bin:$PATH"
export LD_LIBRARY_PATH=/usr/local/opt/openssl/lib:$LD_LIBRARY_PATH
export CPATH=/usr/local/opt/openssl/include:$LD_LIBRARY_PATH

#mysqlenvsパス追加
export PATH="/Users/mars_tran/.mysqlenv/bin:/Users/mars_tran/.mysqlenv/shims:/Users/mars_tran/.mysqlenv/mysql-build/bin:$PATH"

#JAVAのホームパスを定義する
export JAVA_HOME=$(/usr/libexec/java_home)

#Gitのパスをグローバルに追加する
export PATH="/usr/local/Cellar/git/2.8.3/bin:$PATH"

#個人のプロンプトを変更する
#add-zsh-hook precmd battery

#Setting in tmux mode
if [ ! -z ${TMUX} ]; then
    #Inside TMUX
    RPROMPT="%D-%*"
    #Bash PROMPT_COMMANDをzshで実行できるように
    #add-zsh-hook precmd tvd_exec_cmd_list
else
    #Outside of TMUX
    RPROMPT='%D-%*'
fi

export FONTCONFIG_PATH=/opt/X11/lib/X11/fontconfig
test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"
export LESSOPEN="| /usr/local/Cellar/source-highlight/3.1.8_1/bin/src-hilite-lesspipe.sh %s"
export LESS='-RNC'

if [ -n "$TMUX" ]; then
    alias pbcopy="reattach-to-user-namespace pbcopy"
fi

# Docker
eval $(docker-machine env dev)
#export EDITOR=vim
eval "$(direnv hook zsh)"
