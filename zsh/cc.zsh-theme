CC_RED='160'
CC_GREEN='40'
CC_GRAY='240'
CC_WHITE='255'

CC_WRITE_IN_RED="%f%F{$CC_RED}"
CC_WRITE_IN_GREEN="%f%F{$CC_GREEN}"
CC_WRITE_IN_GRAY="%f%F{$CC_GRAY}"
CC_WRITE_ON_WHITE="%k%f%K{$CC_WHITE}%F{$CC_GRAY}"
CC_WRITE_ON_GRAY="%k%f%K{$CC_GRAY}%F{$CC_WHITE}"
CC_WRITE_ON_RED="%k%f%K{$CC_RED}%F{$CC_WHITE}"

CC_DRAW_IN_GREEN_ON_GRAY="%k%f%K{$CC_GRAY}%F{$CC_GREEN}"
CC_DRAW_IN_WHITE_ON_RED="%k%f%K{$CC_RED}%F{$CC_WHITE}"
CC_DRAW_IN_WHITE="%k%f%F{$CC_WHITE}"
CC_DRAW_IN_GRAY="%k%f%F{$CC_GRAY}"
CC_DRAW_IN_RED="%k%f%F{$CC_RED}"

GIT_FRESH_COLOR=$CC_WRITE_IN_GREEN
GIT_STINKS_COLOR=$CC_WRITE_IN_RED
GIT_LOCKED_COLOR=$CC_WRITE_IN_GRAY

CC_SYMBOL_RIGHT_ARROW=$'\ue0b0'
CC_SYMBOL_RIGHT_LIGHT_ARROW=$'\ue0b1'
CC_SYMBOL_LEFT_ARROW=$'\ue0b2'
CC_SYMBOL_LEFT_LIGHT_ARROW=$'\ue0b3'

CC_SYMBOL_CALENDAR=`echo "\u${CODEPOINT_OF_AWESOME_CALENDAR}"`
CC_SYMBOL_TRELLO="\u${CODEPOINT_OF_AWESOME_TRELLO}"

CC_CURRENT_PATH="%1~"
CC_TIME_AND_DATE="%D{%H:%M:%S} $CC_SYMBOL_CALENDAR %D{%Y-%m-%d}"

PROMPT="$CC_WRITE_ON_WHITE $CC_CURRENT_PATH "'$(git_prompt)'" $CC_DRAW_IN_WHITE$CC_SYMBOL_RIGHT_ARROW%f "

if [ "$CC_NO_BLANK_LINE" = "" ]; then
  PROMPT=$'\n'$PROMPT
fi

RPROMPT="$CC_WRITE_IN_WHITE "'$(python_prompt)'

# RPROMPT="$CC_DRAW_IN_RED$CC_SYMBOL_LEFT_ARROW$CC_WRITE_ON_RED"'$(chunkly_prompt)'" $CC_DRAW_IN_WHITE_ON_RED$CC_SYMBOL_LEFT_ARROW$CC_WRITE_ON_WHITE $CC_TIME_AND_DATE "
