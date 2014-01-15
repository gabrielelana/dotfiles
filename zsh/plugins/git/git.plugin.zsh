GIT_AHEAD_SYMBOL="\u${CODEPOINT_OF_AWESOME_ARROW_UP}"
GIT_BEHIND_SYMBOL="\u${CODEPOINT_OF_AWESOME_ARROW_DOWN}"
GIT_DIVERGED_SYMBOL="\u${CODEPOINT_OF_AWESOME_CARET_LEFT}\u${CODEPOINT_OF_AWESOME_CARET_RIGHT}"
GIT_UNTRACKED_SYMBOL="\u${CODEPOINT_OF_AWESOME_UNLINK}"
GIT_DETACHED_SYMBOL="\u${CODEPOINT_OF_AWESOME_LOCK}"
GIT_UNCHANGED_SYMBOL="\u00b7"
GIT_CHANGES_SYMBOL="\u${CODEPOINT_OF_AWESOME_STAR}"
GIT_STASHED_SYMBOL="\u${CODEPOINT_OF_AWESOME_BEAKER}"
GIT_TOCOMMIT_SYMBOL="\u${CODEPOINT_OF_AWESOME_SHOPPING_CART}"
GIT_CONFLICT_SYMBOL="\u${CODEPOINT_OF_OCTICONS_DIFF}"
GIT_GITHUB_SYMBOL="\u${CODEPOINT_OF_OCTICONS_OCTOFACE}"
GIT_GIT_SYMBOL="\u${CODEPOINT_OF_OCTICONS_GIT_BRANCH}"


git_is_repository() {
  git rev-parse --git-dir > /dev/null 2>&1
}

git_some_files_to_be_committed() {
  local -a to_be_committed
  to_be_committed=($(git diff --name-only --staged))
  [ $#to_be_committed -gt 0 ]
}

git_some_files_are_untracked() {
  local -a untracked
  untracked=($(git ls-files --others --exclude-standard))
  [ $#untracked -gt 0 ]
}

git_some_files_are_unmerged() {
  [ $(git_number_of_files_to_merge) -gt 0 ]
}

git_some_files_are_modified() {
  local -a modified
  modified=($(git ls-files --modified))
  [ $#modified -gt 0 ]
}

git_number_of_files_to_merge() {
  local -a unmerged
  unmerged=($(git ls-files --unmerged | cut -d $'\t' -f 2 | uniq))
  print $#unmerged
}

git_seconds_since_last_commit() {
  if git rev-parse --git-dir > /dev/null 2>&1; then
    if [[ $(git log 2>&1 > /dev/null | grep -c "^fatal: bad default revision") == 0 ]]; then
      local now=`date +%s`
      local last_commit=`git log --pretty=format:'%at' -1 2> /dev/null`
      print $((now-last_commit))
    else
      print 0
    fi
  fi
}

git_repository_stinks() {
  local seconds_since_last_commit=${1:-$(git_seconds_since_last_commit)}
  local seconds_of_freshness=$((${CHUNK_DURATION:-1500} * 4))
  local dirty="$(git status --ignore-submodules=none --porcelain 2> /dev/null)"
  [ -n "$dirty" ] || [ $seconds_since_last_commit -gt $seconds_of_freshness ]
}

git_remote_status() {
  remote=${$(git rev-parse --verify ${hook_com[branch]}@{upstream} --symbolic-full-name 2>/dev/null)/refs\/remotes\/}
  if [[ -n ${remote} ]] ; then
    ahead=$(git rev-list ${hook_com[branch]}@{upstream}..HEAD 2>/dev/null | wc -l)
    behind=$(git rev-list HEAD..${hook_com[branch]}@{upstream} 2>/dev/null | wc -l)

    if [ $ahead -eq 0 ] && [ $behind -gt 0 ]
    then
      print " $GIT_BEHIND_SYMBOL ($behind)"
    elif [ $ahead -gt 0 ] && [ $behind -eq 0 ]
    then
      print " $GIT_AHEAD_SYMBOL ($ahead)"
    elif [ $ahead -gt 0 ] && [ $behind -gt 0 ]
    then
      print " $GIT_DIVERGED_SYMBOL "
    fi
  else
    print " $GIT_UNTRACKED_SYMBOL "
  fi
}

git_stash_status() {
  STASH_COUNT=$(git stash list 2>/dev/null | wc -l | tr -d ' ')
  if [ $STASH_COUNT -gt 0 ]; then
    print " $GIT_STASHED_SYMBOL ($STASH_COUNT)"
  else
    print ""
  fi
}

git_remote_origin_is_github() {
  git remote --verbose | grep 'origin.\+github.com' > /dev/null
}

git_time_status() {
  local seconds_since_last_commit=${1:-$(git_seconds_since_last_commit)}
  if [ $seconds_since_last_commit -gt 60 ]; then
    print $(format_seconds_as_human_readable $seconds_since_last_commit)
  else
    print ""
  fi
}

git_prompt() {
  if git_is_repository; then
    local seconds_since_last_commit=$(git_seconds_since_last_commit)
    local time_status="$(git_time_status $seconds_since_last_commit)"
    local remote_status="$(git_remote_status)"

    # maybe add github logo
    local separator="$GIT_GIT_SYMBOL "
    if $(git_remote_origin_is_github); then
      separator="$GIT_GITHUB_SYMBOL  "
    fi

    # fresh or stinks?
    local color=$GIT_FRESH_COLOR
    if $(git_repository_stinks $seconds_since_last_commit); then
      color=$GIT_STINKS_COLOR
    fi

    # something to merge?
    if git_some_files_are_unmerged; then
      merge_status=" $GIT_CONFLICT_SYMBOL $(git_number_of_files_to_merge)"
    fi

    # something changed?
    local change_status=""
    if git_some_files_to_be_committed; then
      change_status=" $GIT_TOCOMMIT_SYMBOL "
    fi
    if git_some_files_are_modified || git_some_files_are_untracked; then
      if [ -n "$change_status" ]; then
        change_status+="$GIT_CHANGES_SYMBOL "
      else
        change_status+=" $GIT_CHANGES_SYMBOL "
      fi
    fi
    if [ -z "$change_status" ] && [ -n "$time_status" ]; then
      change_status="$GIT_UNCHANGED_SYMBOL"
    fi

    # something stashed?
    local stash_status="$(git_stash_status)"

    # not in a symbolic HEAD?
    ref=$(git symbolic-ref HEAD 2> /dev/null)
    if [ -z $ref ]; then
      color=$GIT_LOCKED_COLOR
      remote_status=" $GIT_DETACHED_SYMBOL "
      ref=$(git rev-parse --short HEAD 2> /dev/null)
    fi
    ref=${ref#refs/heads/}

    # finally here is the prompt
    print "$color$separator$ref$change_status$time_status$remote_status$merge_status$stash_status"
  fi
}
