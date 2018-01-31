which thefuck > /dev/null 2>&1
if [ "$?" == "0" ]; then
	eval $(thefuck --alias)
fi
