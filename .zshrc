# fpath=(~/.zsh/completion $fpath)
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt append_history
setopt extended_history
setopt share_history
setopt hist_ignore_dups
setopt hist_ignore_all_dups
setopt auto_cd
setopt auto_pushd
setopt magic_equal_subst
setopt print_eight_bit

# rootは履歴を残さないようにする
if [ $UID = 0 ]; then
    unset HISTFILE
    SAVEHIST=0
fi

# ## zplug setting https://github.com/zplug/zplug
source ~/.zplug/init.zsh
zplug "zsh-users/zsh-syntax-highlighting"

# zplug "zsh-users/zsh-history-substring-search"
# zplug "zsh-users/zsh-completions"
zplug "zsh-users/zsh-autosuggestions"
zplug "b4b4r07/enhancd", use:init.sh
export ENHANCD_FILTER=fzf
# zplug "junegunn/fzf", as:command, use:bin/fzf-tmux
zplug "junegunn/fzf-bin", as:command, from:gh-r, rename-to:fzf
# zplug "themes/steeef", from:oh-my-zsh #themes
zplug "plugins/git", from:oh-my-zsh, as:plugin
zplug "plugins/common-aliases", from:oh-my-zsh, as:plugin
zplug "plugins/emacs", from:oh-my-zsh, as:plugin
zplug load

# if zplug check "zsh-users/zsh-history-substring-search"; then
#   bindkey '^P' history-substring-search-up
#   bindkey '^N' history-substring-search-down
# fi

# # Prevent duplicate defined when use tmux
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
  # export PATH="/usr/local/opt/imagemagick@6/bin:$PATH"
  # export PATH="/usr/local/opt/qt@5.5/bin:$PATH"
  export PATH="$HOME/Library/Android/sdk/platform-tools:$PATH"
  export PATH="$HOME/go/bin:$PATH"
  export PATH="/usr/local/sbin:$PATH"
  eval "$(anyenv init - --no-rehash)"
  eval "$(pyenv virtualenv-init -)"
fi

eval "$(direnv hook zsh)"

source "${HOME}/.iterm2_shell_integration.zsh"
if type 'aws_completer' > /dev/null; then
  source $(dirname `which aws_completer`)/aws_zsh_completer.sh
fi
source $HOME/dotfiles/customized.zsh-theme

export GOPATH="$HOME/go"
export FZF_DEFAULT_COMMAND='ag --hidden --ignore .git -g ""'
export LESS='-gj10 -RNC'
# export LESSOPEN="|$HOME/dotfiles/src-hilite-lesspipe.sh %s"
# zprof
