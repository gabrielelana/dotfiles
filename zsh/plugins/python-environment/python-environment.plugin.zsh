_in_python_virtual_environment() {
  if [ -n "$VIRTUAL_ENV" ]; then
    return 0
  fi
  return 1
}

_python_virtual_environment_prompt_info() {
  if [ -f "$VIRTUAL_ENV/__name__" ]; then
    local name=`cat $VIRTUAL_ENV/__name__`
  elif [ `basename $VIRTUAL_ENV` = "__" ]; then
    local name=$(basename $(dirname $VIRTUAL_ENV))
  else
    local name=$(basename $VIRTUAL_ENV)
  fi
  echo ${name##.}
}

_python_version_prompt_info() {
  echo $(python --version | cut -f2 -d' ')
}

python_prompt() {
  if _in_python_virtual_environment; then
    echo "($(_python_virtual_environment_prompt_info)\u${CODEPOINT_OF_DEVICONS_PYTHON} $(_python_version_prompt_info)) "
  fi
}
