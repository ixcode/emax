#!/bin/bash

echo "Ok, see you later, going to restore your old emacs config..."

read -p "Are you sure (Y/N)? " -n 1 -r
if [[ ${REPLY} =~ ^[Nn]$ ]]; then
    echo -e "\n\nNo worries, just run me again if you change your mind. Nothing happens."
    exit 0
fi

unlink ~/.emacs.d

echo -e "Restoring previos emacs..."

source .emax_restore
ln -svf ${EMACS_D_SYMLINK_TARGET} ~/.emacs.d

echo "Ok, all good, restart emacs!"

