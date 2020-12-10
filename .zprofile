function fcap() {
  app=$(ls -1 config/deploy | grep -v -E '.rb$' |fzf)
  printf "$app was selected to $@? Should i continue that? [y/N]:"
  read confirm
  case $confirm in
    y)
      bundle exec cap $app $@
      ;;
    *)
      echo Nothing had done. Bye!!!
      ;;
  esac
}

function peridot_test() {
  local usage_msg="Usage:\n\t$0 docker_container_name [spec_directory_name]"
  if [ -e $1 ]; then
    echo $usage_msg
    return 1
  fi
  local container_name=$1
  local test_dir=${2:-spec}

  if [ -e $PERIDOT_TEST_CMD ]; then
     echo 'Error: Environment PERIDOT_TEST_CMD is not defined.\n'
     return 1
  fi

  docker exec -it $container_name sh -c "$PERIDOT_TEST_CMD $test_dir"
}

## fzf-tweak (https://github.com/junegunn/fzf/wiki/examples#git)

## Crtl-Rで履歴検索 (fzf)
function fzf-history-selection() {
    BUFFER=$(history -n -r 1 | fzf --reverse --no-sort +m --color 16 --query "$LBUFFER" --prompt="History > ")
    CURSOR=$#BUFFER
    zle reset-prompt
}
zle -N fzf-history-selection
bindkey '^R' fzf-history-selection

fshow() {
  git log --graph --color=always \
      --format="%C(auto)%h%d %s %C(black)%C(bold)%cr" "$@" |
  fzf --ansi --no-sort --reverse --tiebreak=index --bind=ctrl-s:toggle-sort \
      --bind "ctrl-m:execute:
                (grep -o '[a-f0-9]\{7\}' | head -1 |
                xargs -I % sh -c 'git show --color=always % | less -R') << 'FZF-EOF'
                {}
FZF-EOF"
}

fcs() {
  local commits commit
  commits=$(git log --color=always --pretty=oneline --abbrev-commit --reverse) &&
  commit=$(echo "$commits" | fzf --tac +s +m -e --ansi --reverse) &&
  echo -n $(echo "$commit" | sed "s/ .*//")
}

fbr() {
  local branches branch
  # branches=$(git for-each-ref --count=30 --sort=-committerdate refs/heads/ --format="%(refname:short)") &&
  # branch=$(echo "$branches" |
  #          fzf-tmux -d $(( 2 + $(wc -l <<< "$branches") )) +m) &&
  branches=$(git branch --all | grep -v HEAD) &&
  branch=$(echo "$branches" |
           fzf-tmux -d $(( 2 + $(wc -l <<< "$branches") )) +m) &&
  git checkout $(echo "$branch" | sed "s/.* //" | sed "s#remotes/[^/]*/##")
}

fkill() {
  local pid
  pid=$(ps -ef | sed 1d | fzf -m | awk '{print $2}')

  if [ "x$pid" != "x" ]
  then
    echo $pid | xargs kill -${1:-9}
  fi
}

function git() {
  if (which hub 1>/dev/null); then
    hub "$@"
  else
    /usr/bin/git "$@"
  fi
}

showSSH() {
    local search_options=$1
    for item in `find $HOME/.ssh/ -type f -name '*.conf' -o -name 'config'` ; do
        local result=$(gsed -n -e "/^Host $search_options.*/,/^\n$/p;/^\n$/q" $item)
        if [ ! $result = '' ]; then
          echo "----- IN $item -----"
          echo $result
        fi
    done
}

safe_ssh() {
  local mode=${1:-on}
  local on_regexp='s/^\(Include .*\)/#\1/g'
  local off_regexp='s/^#\(Include .*\)/\1/g'
  local config_path=$HOME/.ssh/config
  [ $mode = 'on' ]  && gsed -i -e $on_regexp $config_path  && echo 'All SSH settings is OFF' && return 0;
  [ $mode = 'off' ] && gsed -i -e $off_regexp $config_path && echo '!!! ALL SSH settings is ON !!!' && return 0;
  echo 'Usage: safe_ssh [on|off]'
  return 1;
}

alias fssh='ssh $(echo `cat ~/.ssh/config ~/.ssh/conf.d/*.conf|gsed -n -e "/^Host / { s/^Host \(.\+\)$/\1/g; /\*/d; s/ /\n/g;p }"|fzf --reverse`)'
alias fdc='docker container ls -a|fzf -m --reverse|cut -d " " -f1|sed -e ":a" -e "N" -e "$!ba" -e "s/\n/ /g"'
alias revert-tree="sort|gsed -e '1d; s/^\.//;s/\/\([^/]*\)$/|--\1/;s/\/[^/|]*/|  /g'"
alias rcolor="gsed -r 's/\x1B\[[0-9;]+[mGK]//g'"
alias tree='tree -FANC --dirsfirst'
alias E="SUDO_EDITOR=\"emacsclient\" sudo -e"
# TMUX direnv https://github.com/direnv/direnv/wiki/Tmux
alias tmux="direnv exec / tmux"
alias fe='emacsclient -nw $(fzf +m --reverse --preview "less {}")'
alias pcd='cd "$(ghq list -p|fzf)"'
alias pip="pip3"
alias docker-cleanup="docker container ls -a -q --filter status=exited|xargs docker container rm"
case "${OSTYPE}" in
darwin*)
  source $HOME/.config/.zprofile_darwin
  ;;
linux*)
  source $HOME/.config/.zprofile_linux
  ;;
esac
