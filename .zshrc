HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
CONFIG_DIR=$HOME/.config
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
## zplug setting https://github.com/zplug/zplug
export ZPLUG_HOME=/usr/local/opt/zplug
export PATH="$PATH:/usr/local/bin/"
if [ -e $ZPLUG_HOME/init.zsh ]; then
  source $ZPLUG_HOME/init.zsh
  zplug "zsh-users/zsh-syntax-highlighting", defer:2
  zplug "zsh-users/zsh-history-substring-search"
  zplug "zsh-users/zsh-completions"
  zplug "zsh-users/zsh-autosuggestions"
  zplug "b4b4r07/enhancd", use:init.sh
  export ENHANCD_FILTER=fzf
  zplug "plugins/git", from:oh-my-zsh, as:plugin
  zplug "plugins/common-aliases", from:oh-my-zsh, as:plugin
  zplug "plugins/emacs", from:oh-my-zsh, as:plugin
  # zplug 'zplug/zplug', hook-build:'zplug --self-manage'
  zplug load
else
  echo 'NO ZPLUG :('
fi
# bindkey -M emacs '^P' history-substring-search-up
# bindkey -M emacs '^N' history-substring-search-down

# load theme
source $ZDOTDIR/customized.zsh-theme

# # Prevent duplicate defined when use tmux
typeset -U path PATH
if [ -z $TMUX ]; then
  export PATH="$HOME/.anyenv/bin:$PATH"
  export ANYENV_ROOT="$HOME/.anyenv"
  export PATH=$PATH:/usr/local/share/git-core/contrib/diff-highlight
  export PATH="$HOME/Library/Android/sdk/platform-tools:$PATH"
  export PATH="$HOME/go/bin:$PATH"
  export PATH="/usr/local/sbin:$PATH"
  export PATH="/usr/local/aws/bin/:$PATH"
fi
eval "$(anyenv init - --no-rehash)"
eval "$(pyenv virtualenv-init -)"
eval "$(direnv hook zsh)"

typeset -U path PATH
export FPATH=$FPATH:$HOME/.config/.zsh/completion

if [ -e $CONFIG_DIR/iterm2/iterm2_shell_integration.zsh ]; then
  source $CONFIG_DIR/iterm2/iterm2_shell_integration.zsh
fi

export FZF_DEFAULT_COMMAND='ag --hidden --ignore .git -g ""'
export LESS='-gj10 -RNC'
export LESSOPEN="|$HOME/dotfiles/less_pygmentize.sh %s"
# alias tmux='direnv exec / tmux'

# The following lines were added by compinstall
zstyle ':completion:*' completer _expand _complete _ignored _correct _approximate
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' menu select=2
zstyle ':completion:*' max-errors 1
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl true
zstyle ':completion:*' verbose true
zstyle ':completion:*:manuals' separate-sections true
zstyle :compinstall filename '/Users/mars_tran/dotfiles/.zshrc'

autoload -Uz compinit
compinit -i
# End of lines added by compinstall

if [ -e /usr/local/aws/bin/aws_zsh_completer.sh ]; then
  source /usr/local/aws/bin/aws_zsh_completer.sh
fi
export ZSH_AUTOSUGGEST_USE_ASYNC=true
export ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20
if [ ~/dotfiles/.zshrc -nt ~/dotfiles/.zshrc.zwc ]; then
  zcompile ~/dotfiles/.zshrc
fi
export GPG_TTY=$(tty)
