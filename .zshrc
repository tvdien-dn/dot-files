# the following lines were added by compinstall
fpath=(/Applications/Docker.app/Contents/Resources/etc/ $fpath)
fpath=(~/.zsh/completion $fpath)

zstyle ':completion:*' completer _expand _complete _ignored _correct _approximate _options
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' max-errors 2
zstyle ':completion:*' menu select=2
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl true
zstyle ':completion:*' verbose true
zstyle ':completion:*:manuals' separate-sections true
zstyle :compinstall filename '/Users/mars_tran/dotfiles/.zshrc'

autoload -Uz compinit && compinit -i

# zstyle ':completion:*' menu select
# zstyle ':completion:*:cd:*' ignore-parents parent pwd
# zstyle ':completion:*:descriptions' format '%BCompleting%b %U%d%u'

# SSHのホスト補完データ
# _cache_hosts=(`ruby -ne 'if /^Host\s+(.+)$/; print $1.strip, "\n"; end' ~/.ssh/conf.d/*.conf`)
# _cache_hosts=(`grep -E '^Host ' ~/.ssh/config ~/.ssh/conf.d/*.conf|cut -d ' ' -f 2`)
# End of lines added by compinstall
# Lines configured by zsh-newuser-install
HISTFILE=~/.zsh_history
HISTSIZE=1000000
SAVEHIST=1000000
##コマンド履歴検索
setopt append_history
setopt extended_history
setopt share_history
setopt hist_ignore_dups
setopt hist_ignore_all_dups
setopt auto_cd
setopt auto_pushd
# =の後にパス補完
setopt magic_equal_subst

## 日本語ファイル名を表示可能にする
setopt print_eight_bit

# rootは履歴を残さないようにする
if [ $UID = 0 ]; then
    unset HISTFILE
    SAVEHIST=0
fi

## zplug setting https://github.com/zplug/zplug
source ~/.zplug/init.zsh
zplug "zsh-users/zsh-syntax-highlighting"
zplug "zsh-users/zsh-history-substring-search"
zplug "zsh-users/zsh-completions"
zplug "zsh-users/zsh-autosuggestions"
# zplug "mollifier/anyframe"
zplug "b4b4r07/enhancd", use:init.sh
export ENHANCD_FILTER=fzf
# zplug "b4b4r07/emoji-cli"
# export EMOJI_CLI_KEYBIND=
# export EMOJI_CLI_FILTER=fzf
zplug "junegunn/fzf", as:command, use:bin/fzf-tmux
zplug "junegunn/fzf-bin", as:command, from:gh-r, rename-to:fzf
zplug "themes/steeef", from:oh-my-zsh #themes
# zplug "peco/peco", as:command, from:gh-r
zplug "plugins/git", from:oh-my-zsh, as:plugin
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

## 色付きlessコマンド
export LESSOPEN="|$HOME/dotfiles/src-hilite-lesspipe.sh %s"
export LESS='-gj10 -RNC'

##alias to mysqlversion 5.7.9
alias mycli57='mycli -uroot -P3357 --prompt="\u@\h:\d\n>"'
alias mycli56='mycli -u root -P3356 --prompt="\u@\h:\d\n>"'

# Prevent duplicate defined when use tmux
if [ -z $TMUX ]
then
    #Anyenv 設定
    export PATH="$HOME/dotfiles/.anyenv/bin:$PATH"
    export ANYENV_ROOT="$HOME/dotfiles/.anyenv"
    # AWS-CLI
    export PATH=~/.local/bin:$PATH
    # GIT COLORFUL
    export PATH=$PATH:/usr/local/share/git-core/contrib/diff-highlight
    # Brew, Imagemagick
    export PATH="/usr/local/opt/imagemagick@6/bin:$PATH"
    export PATH="$(brew --prefix qt@5.5)/bin:$PATH"
fi
eval "$(anyenv init -)"
eval "$(pyenv virtualenv-init -)"

# load tmuxinator completion
source /Users/mars_tran/dotfiles/.anyenv/envs/rbenv/versions/2.4.1/lib/ruby/gems/2.4.0/gems/tmuxinator-0.10.1/completion/tmuxinator.zsh

#alias to use emacsclient
alias e='myeditor $@'
#alias quick reset emacs server
alias emacsreset='emacsclient -e "(kill-emacs)" && emacs --daemon'

# direnv
eval "$(direnv hook zsh)"

source "${HOME}/.iterm2_shell_integration.zsh"
iterm2_print_user_vars() {
  iterm2_set_user_var hoge $(echo "hgoe")
  iterm2_set_user_var gitBranch $((git branch 2> /dev/null) | grep \* | cut -c3-)
}

# Turn on AWS CLI auto-completion
if type 'aws_completer' > /dev/null; then
  source $(dirname `which aws_completer`)/aws_zsh_completer.sh
  # source /usr/local/bin/aws_zsh_completer.sh
fi
export PATH="$HOME/Library/Android/sdk/platform-tools:$PATH"
source $HOME/dotfiles/customized.zsh-theme
