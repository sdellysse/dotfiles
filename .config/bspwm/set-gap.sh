#!/bin/sh
set -euo pipefail

set_gap(){
  local GAP_PX="$1"

  local DEFAULT_PANEL_SIZE=24

  local PANEL_NAMES="$( dconf read /org/mate/panel/general/toplevel-id-list | sed 's/^\[//g' | sed 's/\]$//g' | sed "s/',\\s*/\n/g" | sed "s/'//g" )"

  local T_PANEL_PX=0
  local B_PANEL_PX=0
  local L_PANEL_PX=0
  local R_PANEL_PX=0

  while read NAME; do
    local ORIENTATION="$( dconf read "/org/mate/panel/toplevels/$NAME/orientation" | sed "s/'//g" )"
    local DCONF_SIZE="$( dconf read "/org/mate/panel/toplevels/$NAME/size" )"
    local PANEL_SIZE="${DCONF_SIZE:-$DEFAULT_PANEL_SIZE}"

    case "$ORIENTATION" in
      "top")
        T_PANEL_PX="$PANEL_SIZE"
      ;;

      "bottom")
        B_PANEL_PX="$PANEL_SIZE"
      ;;

      "left")
        L_PANEL_PX="$PANEL_SIZE"
      ;;

      "right")
        R_PANEL_PX="$PANEL_SIZE"
      ;;
    esac
  done <<< "$PANEL_NAMES"


  bspc config window_gap "$GAP_PX"
  bspc config top_padding    "$( expr "$T_PANEL_PX" - "$GAP_PX" )"
  bspc config bottom_padding "$( expr "$B_PANEL_PX" - "$GAP_PX" )"
  bspc config left_padding   "$( expr "$L_PANEL_PX" - "$GAP_PX" )"
  bspc config right_padding  "$( expr "$L_PANEL_PX" - "$GAP_PX" )"
}

if [ "$#" = "2" ]; then
  DIRECTION="$1"
  AMOUNT="$2"

  CURRENT="$( bspc config window_gap )"

  set_gap "$( expr "$CURRENT" "$DIRECTION" "$AMOUNT" )"
  exit
fi

if [ "$#" = "1" ]; then
  SIZE="$1"

  set_gap "$SIZE"
  exit
fi

echo "Bad arguments"
exit 1
