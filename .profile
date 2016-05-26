#!/bin/sh
if [ -n "$_PROFILE_RAN" ]; then return; fi
_PROFILE_RAN=$(date)

if [ -n "${BASH_VERSION}" ]; then
    source ${HOME}/.profile.d/bash/profile
fi
if [ -e /Users/shawnd/.nix-profile/etc/profile.d/nix.sh ]; then . /Users/shawnd/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
