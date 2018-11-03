export VIRTUAL_ENV_DISABLE_PROMPT=1

function virtualenv_info {
    [ $VIRTUAL_ENV ] && echo '('%F{blue}`basename $VIRTUAL_ENV`%f') '
}

setopt prompt_subst

autoload -U add-zsh-hook
autoload -Uz vcs_info

#use extended color palette if available
if [[ $terminfo[colors] -ge 256 ]]; then
    turquoise="%F{81}"
    orange="%F{166}"
    purple="%F{135}"
    hotpink="%F{161}"
    limegreen="%F{118}"
else
    turquoise="%F{cyan}"
    orange="%F{yellow}"
    purple="%F{magenta}"
    hotpink="%F{red}"
    limegreen="%F{green}"
fi

# enable VCS systems you use
zstyle ':vcs_info:*' enable git svn

# check-for-changes can be really slow.
# you should disable it, if you work with large repositories
zstyle ':vcs_info:*:prompt:*' check-for-changes true

# set formats
# %b - branchname
# %u - unstagedstr (see below)
# %c - stagedstr (see below)
# %a - action (e.g. rebase-i)
# %R - repository path
# %S - path in the repository
PR_RST="%f"
FMT_BRANCH="(%{$turquoise%}%b%u%c${PR_RST})"
FMT_ACTION="(%{$limegreen%}%a${PR_RST})"
FMT_UNSTAGED="%{$orange%}✔︎"
FMT_STAGED="%{$limegreen%}✔︎"

zstyle ':vcs_info:*:prompt:*' unstagedstr   "${FMT_UNSTAGED}"
zstyle ':vcs_info:*:prompt:*' stagedstr     "${FMT_STAGED}"
zstyle ':vcs_info:*:prompt:*' actionformats "${FMT_BRANCH}${FMT_ACTION}"
zstyle ':vcs_info:*:prompt:*' formats       "${FMT_BRANCH}"
zstyle ':vcs_info:*:prompt:*' nvcsformats   ""

function theme_precmd {
    if git ls-files --other --exclude-standard 2> /dev/null | grep -q "."; then
        FMT_BRANCH="(%{$turquoise%}%b%u%c%{$hotpink%}✔︎${PR_RST})"
    else
        FMT_BRANCH="(%{$turquoise%}%b%u%c${PR_RST})"
    fi
    zstyle ':vcs_info:*:prompt:*' formats "${FMT_BRANCH} "

    vcs_info 'prompt'
}
add-zsh-hook precmd theme_precmd
local current_dir=
# Override prompt settings in steeef's theme
PROMPT=$'%{$purple%}%n${PR_RST}@%{$orange%}%m${PR_RST}:%{$limegreen%}%1~${PR_RST} $vcs_info_msg_0_$(virtualenv_info)
%B[%D{%b/%e %H:%M}]%b >> '
