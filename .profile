#!/bin/sh
if [ -n "$_PROFILE_RAN" ]; then return; fi
_PROFILE_RAN=$(date)

if [ -n "${BASH_VERSION}" ]; then
    source ${HOME}/.profile.d/bash/profile
fi
