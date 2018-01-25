if [ -z "${DOTBASH_os}" ]; then
  UNAME_S=$(uname -s)

  if echo "$UNAME_S" | grep -q 'Darwin'; then
      DOTBASH_os="macos"
  elif echo "$UNAME_S" | grep -q "Linux"; then
      DOTBASH_os="linux"
  elif echo "$UNAME_S" | grep -q 'SunUNAME_S'; then
      DOTBASH_os="solaris"
  elif echo "$UNAME_S" | grep -q 'SunOS'; then
      DOTBASH_os="sunos"
  elif echo "$UNAME_S" | grep -q 'CYGWIN'; then
      DOTBASH_os="windows"
  else
      DOTBASH_os="unknown"
  fi
fi

if [ -z "${DOTBASH_host}" ]; then
	export DOTBASH_host="$( hostname )"
fi

if [ -z "${DOTBASH_user}" ]; then
	export DOTBASH_user="${USER}"
fi

if [ -z "${DOTBASH_search_paths}" ]; then
	DOTBASH_oldIFS="$IFS"
	IFS="$( echo -en "\n\b" )"

	declare -a DOTBASH_search_paths_array=(
		"${DOTBASH_user}@${DOTBASH_host}@${DOTBASH_os}"
		"any@${DOTBASH_host}@${DOTBASH_os}"
		"any@any@${DOTBASH_os}"
		"any@any@any"
	)
	export DOTBASH_search_paths="${DOTBASH_search_paths_array[*]}"

	IFS="$DOTBASH_oldIFS"
	unset DOTBASH_search_paths_array
	unset DOTBASH_oldIFS
fi
