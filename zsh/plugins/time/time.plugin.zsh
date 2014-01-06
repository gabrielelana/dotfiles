speed_of() {
  local start=$(date +%s%N) 
  echo $($1) > /dev/null
  local stop=$(date +%s%N) 
  print $((($stop - $start) / 1000000.0))
}

format_seconds_as_human_readable() {
  local seconds_since=${1:-0}

  local minutes_since=$((seconds_since / 60))
  local hours_since=$((seconds_since / 3600))
  local days_since=$((seconds_since / 86400))
  local minutes=$((minutes_since % 60))

  if [ $hours_since -gt 24 ]; then
    print "${days_since}d"
  elif [ $minutes_since -gt 60 ]; then
    print "${hours_since}h${minutes}m"
  elif [ $minutes_since -gt 0 ]; then
    print "${minutes_since}m"
  elif [ $seconds_since -gt 0 ]; then
    print "${seconds_since}s"
  else
    print "now"
  fi
}

format_seconds_as_clock() {
  local seconds_since=${1:-0} 
  printf "%02d:%02d" $((seconds_since/60)) $(($seconds_since%60))
}
