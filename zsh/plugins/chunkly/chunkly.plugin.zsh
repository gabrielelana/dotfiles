CHUNKLY_CHUNK_DURATION=1500

CHUNKLY_POMODORO_TICKING="\u${CODEPOINT_OF_POMICONS_POMODORO_TICKING}"
CHUNKLY_POMODORO_DONE="\u${CODEPOINT_OF_POMICONS_POMODORO_DONE}"
CHUNKLY_BREAK_SYMBOL="\u${CODEPOINT_OF_AWESOME_COFFEE}"
CHUNKLY_LONG_BREAK_SYMBOL="\u${CODEPOINT_OF_POMICONS_LONG_PAUSE}"
CHUNKLY_AWAY_SYMBOL="\u${CODEPOINT_OF_POMICONS_AWAY}"
CHUNKLY_VACATION_SYMBOL="\u${CODEPOINT_OF_POMICONS_LONG_PAUSE}"
CHUNKLY_SQUASHED_SYMBOL="\u${CODEPOINT_OF_POMICONS_POMODORO_SQUASHED}"
CHUNKLY_LABEL_SYMBOL="\u${CODEPOINT_OF_AWESOME_TAG}"
CHUNKLY_PAIR_SYMBOL="\u${CODEPOINT_OF_POMICONS_PAIR_PROGRAMMING}"

chunkly_push() {
  rsync -avz -e ssh ~/.chunkly cleancode.it:/root/home/gabrielelana
}

chunkly_pull() {
  rsync -avz -e ssh cleancode.it:/root/home/gabrielelana/.chunkly ~
}


chunkly_edit() {
  local at_day=`date --date="${*:-today}" +"%Y-%m-%d" 2> /dev/null`
  local log_of_the_day="$HOME/.chunkly/$at_day.log"
  local local_vimrc="$HOME/.chunkly/.vimrc"

  if [ -f $log_of_the_day ]; then
    if [ -f $local_vimrc ]; then
      $EDITOR -c ":source $local_vimrc" $log_of_the_day
    else
      $EDITOR $log_of_the_day
    fi
  else
    echo "You did nothing ${*}, you should be proud of yourself..."
  fi
}

chunkly_cat() {
  local at_day=`date --date="${*:-today}" +"%Y-%m-%d" 2> /dev/null`
  local log_of_the_day="$HOME/.chunkly/$at_day.log"

  if [ -f $log_of_the_day ]; then
    cat $log_of_the_day
  else
    echo "You did nothing ${*}, you should be proud of yourself..."
  fi
}

chunkly_show() {
  local at_day=`date --date="${*:-today}" +"%Y-%m-%d"`
  local beginning_of_the_day=`date --date="$at_day" +"%s"`
  local end_of_the_day=`date --date="$at_day 1 day" +"%s"`
  local log_of_the_day="$HOME/.chunkly/$at_day.log"

  pomodori_of_the_day=()
  for start_time in `cat $log_of_the_day | grep 'start' | cut -f 3 | sort`; do
    start_time=${start_time/T/ }
    started_at=`date --date="$start_time" +"%s"`
    pomodori_of_the_day+=($started_at)
  done

  start_of_the_first_pomodoro=$pomodori_of_the_day[1]
  end_of_the_last_pomodoro=$((pomodori_of_the_day[${#pomodori_of_the_day}] + 25*60))

  start_from=`date --date="@$start_of_the_first_pomodoro" +%k`
  end_to=`date --date="@$end_of_the_last_pomodoro" +%k`
  start_from=$((start_from - 1))
  end_to=$((end_to + 1))
  [ $start_from -lt 0 ] && start_from=0
  [ $end_to -gt 23 ] && end_to=23

  pomodori_of_the_day+=($end_of_the_day)

  local pomodoro_index=1
  local started_at=$pomodori_of_the_day[$pomodoro_index]
  pomodoro_index=$((pomodoro_index + 1))

  local now=0
  local at_minute=0
  local background_color=250
  local time_to_display="00:00"
  local in_pomodoro=false
  local line=''

  echo
  for hours in {$start_from..$end_to}; do
    line=''
    for minutes in {0..59}; do
      now=$((beginning_of_the_day + (hours * 3600) + (minutes * 60)))
      if $in_pomodoro; then
        if [ $at_minute -ge 25 ]; then
          in_pomodoro=false
          started_at=$pomodori_of_the_day[$pomodoro_index]
          pomodoro_index=$((pomodoro_index + 1))
        fi
      else
        if [ $((now/60)) -ge $((started_at/60)) ]; then
          in_pomodoro=true
          time_to_display=`printf " ${CHUNKLY_POMODORO_DONE} %02d:%02d" $hours $minutes`
          display_index=0
          at_minute=0
        fi
      fi
      if $in_pomodoro; then
        if [ $((hours % 2)) -eq 0 ]; then
          backgroud_color=75
          foreground_color=255
        else
          backgroud_color=255
          foreground_color=0
        fi
        if [ $display_index -lt ${#time_to_display} ]; then
          if [ $display_index -lt 1 ] && [ $minutes -gt 56 ]; then
            line="$line\e[48;5;${backgroud_color}m \e[0m"
          else
            line="$line\e[38;5;${foreground_color}m\e[48;5;${backgroud_color}m${time_to_display:$display_index:1}\e[0m"
            display_index=$((display_index + 1))
          fi
        else
          line="$line\e[48;5;${backgroud_color}m \e[0m"
        fi
        at_minute=$((at_minute + 1))
      else
        line="$line "
      fi
    done
    printf "%02d:00 $line\n" $hours
  done
}

chunkly_command() {
  local id="$(mongo --nodb --quiet --eval 'print(new ObjectId().valueOf());')"
  local at="$(date +%Y-%m-%dT%H:%M:%S%:z)"
  local command=${1:-'?'}
  local message=${2:-''}
  echo -e "$id\tL\t$at\t$command\t$message" >> ~/.chunkly/$(date +%Y-%m-%d).log
}

chunkly_working_on() {
  echo -e -n "$1" > ~/.chunkly/working_on
}

chunkly_working_off() {
  rm -f ~/.chunkly/working_on
}

chunkly_working_with() {
  echo -e -n "$1" > ~/.chunkly/working_with
}

chunkly_working_alone() {
  rm -f ~/.chunkly/working_with
}

chunkly_remote_start() {
  local comment=${1-}
  ssh -q starbuck <<-EOC
    cat <<EOS | osascript
      tell application "System Events"
        tell application "Pomodoro"
          start "$comment"
        end tell
        delay 0.2
        tell application "System Events" to keystroke return
        delay 0.2
        set frontmost of process "VirtualBox VM" to true
      end tell
EOS
EOC
}

chunkly_remote_squash() {
  ssh -q starbuck <<-EOC
    cat <<EOS | osascript
      tell application "System Events"
        tell application "Pomodoro"
          reset
        end tell
        set frontmost of process "VirtualBox VM" to true
      end tell
EOS
EOC
}

chunkly_mark_as_started() {
  date --utc +%s >| ~/.chunkly/last_chunk_started_at
}

chunkly_mark_as_squashed() {
  date --utc +%s >| ~/.chunkly/last_chunk_squashed_at
}

chunkly_start() {
  if ! chunkly_is_ticking; then
    local comment=${1-}
    if [ -f ~/.chunkly/working_on ]; then
      local working_on="#$(cat ~/.chunkly/working_on)"
      if [ "${comment:0:1}" != ">" ]; then
        working_on+=' '
      fi
      comment="$working_on$comment"
    fi
    if [ -f ~/.chunkly/working_with ]; then
      local working_with="#with>$(cat ~/.chunkly/working_with)"
      if [ ! "$comment" =~ ".+--.+" ]; then
        working_with="-- $working_with"
      fi
      comment="$comment $working_with"
    fi
    chunkly_command 'start' "$CHUNKLY_CHUNK_DURATION\t$comment"
    chunkly_remote_start $comment
    chunkly_mark_as_started
  fi
}

chunkly_continue() {
  if ! chunkly_is_ticking; then
    local last_chunk_comment="$(cat ~/.chunkly/$(date +%Y-%m-%d).log | ack '\tstart\t' | tail -n1 | cut -f6)"
    chunkly_command 'start' "$CHUNKLY_CHUNK_DURATION\t$last_chunk_comment"
    chunkly_remote_start $last_chunk_comment
    chunkly_mark_as_started
  fi
}

chunkly_squash() {
  if chunkly_is_ticking; then
    chunkly_command 'squash' ${1:-}
    chunkly_remote_squash
    chunkly_mark_as_squashed
  fi
}

chunkly_last_started_at() {
  started_at=0
  if [[ -f ~/.chunkly/last_chunk_started_at ]]; then
    started_at=`cat ~/.chunkly/last_chunk_started_at`
  fi
  print $started_at
}

chunkly_last_squashed_at() {
  squashed_at=0
  if [[ -f ~/.chunkly/last_chunk_squashed_at ]]; then
    squashed_at=`cat ~/.chunkly/last_chunk_squashed_at`
  fi
  print $squashed_at
}

chunkly_seconds_since_last_started() {
  print $(($(date --utc +%s) - $(chunkly_last_started_at)))
}

chunkly_seconds_since_last_squashed() {
  print $(($(date --utc +%s) - $(chunkly_last_squashed_at)))
}

chunkly_seconds_to_complete() {
  local ss=${1:-$(chunkly_seconds_since_last_started)}
  print $(($CHUNKLY_CHUNK_DURATION - $ss))
}

chunkly_seconds_on_break() {
  local ss=${1:-$(chunkly_seconds_since_last_started)}
  print $(($ss - $CHUNKLY_CHUNK_DURATION))
}

chunkly_is_squashed() {
  local ss=${1:-$(chunkly_seconds_since_last_started)}
  local sq=${2:-$(chunkly_seconds_since_last_squashed)}
  [[ $ss -lt $CHUNKLY_CHUNK_DURATION ]] && [[ $sq -lt $CHUNKLY_CHUNK_DURATION ]] && [[ $ss -gt $sq ]]
}

chunkly_is_ticking() {
  local ss=${1:-$(chunkly_seconds_since_last_started)}
  [[ $ss -lt $CHUNKLY_CHUNK_DURATION ]] && ! chunkly_is_squashed
}

chunkly_on_break() {
  local ss=${1:-$(chunkly_seconds_since_last_started)}
  [[ $ss -ge $CHUNKLY_CHUNK_DURATION ]]
}

chunkly_on_vacation() {
  local ss=${1:-$(chunkly_seconds_since_last_started)}
  [[ $ss -ge $(($CHUNKLY_CHUNK_DURATION * 2)) ]]
}

chunkly_prompt() {
  local prompt=""
  local working_on=""
  local working_with=""
  local ss=$(chunkly_seconds_since_last_started)
  local sq=$(chunkly_seconds_since_last_squashed)
  if $(chunkly_is_ticking $ss); then
    local sc=$(chunkly_seconds_to_complete $ss)
    prompt+=" $CHUNKLY_POMODORO_TICKING $(format_seconds_as_clock $sc)"
  elif $(chunkly_on_break $ss); then
    local sb=$(chunkly_seconds_on_break $ss)
    prompt+=" $CHUNKLY_BREAK_SYMBOL $(format_seconds_as_clock $sb)"
  elif $(chunkly_is_squashed $ss $sq); then
    prompt+=" $CHUNKLY_SQUASHED_SYMBOL $(format_seconds_as_clock $sq)"
  else
    local sb=$(($(chunkly_seconds_on_break $ss) - $CHUNKLY_CHUNK_DURATION))
    prompt+=" $CHUNKLY_VACATION_SYMBOL $(format_seconds_as_human_readable $sb)"
  fi
  if [ -f ~/.chunkly/working_on ]; then
    working_on=" $CHUNKLY_LABEL_SYMBOL $(cat ~/.chunkly/working_on)"
  fi
  if [ -f ~/.chunkly/working_with ]; then
    working_with=" $CHUNKLY_PAIR_SYMBOL"
  fi
  print "$prompt$working_with$working_on"
}
