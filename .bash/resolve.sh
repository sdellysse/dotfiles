#!/bin/sh
wanted_path="$1"

while read -r config; do
	this_path="${HOME}/.bash/${config}/${wanted_path}"
	if [ -e "$this_path" ]; then
		echo "$this_path"
		exit 0
	fi
done <<< "$DOTBASH_search_paths"
exit 1
