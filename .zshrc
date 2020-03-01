HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
CONFIG_DIR=$HOME/.config
setopt append_history
setopt extended_history
setopt share_history
setopt hist_ignore_dups
setopt hist_ignore_space
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
typeset -U path PATH
if [ -z $TMUX ]; then
  export ANYENV_ROOT="$HOME/.anyenv"
  custom_paths=($HOME/.anyenv/bin(N-/)
                $HOME/Library/Android/sdk/platform-tools(N-/)
                $HOME/go/bin(N-/)
                $HOME/.gem/ruby/2.3.0/bin
                $HOME/Library/Python/3.7/bin(N-/)
                $HOME/Library/Python/2.7/bin(N-/)
                /usr/local/sbin(N-/)
                /usr/local/aws/bin(N-/)
                /usr/local/bin(N-/)
                /usr/local/opt/qt@5.5/bin(N-/)
                /usr/local/opt/openssl/bin(N-/)
               )
  export path=($custom_paths $path)
fi

if [ -e $CONFIG_DIR/zplug_init.sh ]; then
   source $CONFIG_DIR/zplug_init.sh
fi

#  pure prompt with time
PROMPT='%F{white}%* '$PROMPT

# Prevent duplicate defined when use tmux
if [ -n "$(which anyenv)" -a "$(which anyenv)" != 'anyenv not found' ]; then
  eval "$(anyenv init - --no-rehash)"
  eval "$(pyenv virtualenv-init -)"
fi
eval "$(direnv hook zsh)"

typeset -U path PATH

export fpath=($fpath $HOME/.config/.zsh/completion(N-/))

if [ -e $HOME/.iterm2_shell_integration.zsh ]; then
  source $HOME/.iterm2_shell_integration.zsh
fi

export FZF_DEFAULT_COMMAND='ag --hidden --ignore .git -g ""'
export FZF_DEFAUTL_OPTS='--layout=reverse-list'
export LESS='-gj10 -RNC'
if [ -f $HOME/.config/bin/less_pygmentize.sh ] && [ ! -z "$(pip --no-python-version-warning list|grep Pygments)" ]; then
  export LESSOPEN="|$HOME/.config/bin/less_pygmentize.sh %s"
fi
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
zstyle :compinstall filename '$HOME/.config/.zshrc'

autoload -Uz compinit
compinit -i
# End of lines added by compinstall

if [ -e $HOME/Library/Python/3.7/bin/aws_zsh_completer.sh ]; then
  source $HOME/Library/Python/3.7/bin/aws_zsh_completer.sh
fi

if which pip 1>/dev/null 2>&1; then
  if [ ! -e $HOME/.config/.zsh/completion/_pip ]; then
    mkdir -p $HOME/.config/.zsh/completion
    pip completion --zsh >> $HOME/.config/.zsh/completion/_pip
  fi
  source $HOME/.config/.zsh/completion/_pip
fi

export ZSH_AUTOSUGGEST_USE_ASYNC=true
export ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=30
export GPG_TTY=$(tty)
export PAGER='LESS= less -FXR'

if [ -f $(brew --prefix)/etc/brew-wrap ];then
  source $(brew --prefix)/etc/brew-wrap
fi

# FIXME: emove alias of find added by zplug/common-aliases
unalias fd 2>/dev/null
unalias ff 2>/dev/null

if [ -d '/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk' ]; then
  source '/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.zsh.inc'
  source '/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/completion.zsh.inc'
fi

if [ ~/.config/.zshrc -nt ~/.config/.zshrc.zwc ]; then
  zcompile ~/.config/.zshrc
fi
