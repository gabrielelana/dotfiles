superscript() {
  local number=${1:-0}
  local superscript=''
  print $number | sed -e 's/\(.\)/\1\n/g' | while read digit; do
    if [[ -n $digit ]]; then
      case "$digit" in
        1) superscript+="\u00b9"
        ;;
        [23]) superscript+=$(printf "\\\u%04x" $(($digit + 0x00b0)))
        ;;
        *) superscript+=$(printf "\\\u%04x" $(($digit + 0x2070)))
        ;;
      esac
    fi
  done
  print $superscript
}

subscript() {
  local number=${1:-0}
  local subscript=''
  print $number | sed -e 's/\(.\)/\1\n/g' | while read digit; do
    if [[ -n $digit ]]; then
      subscript+=$(printf "\\\u%04x" $(($digit + 0x2080)))
    fi
  done
  print $subscript
}
