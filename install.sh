#!/bin/bash

# This first version only works with .emacs.d right now

echo "Going to install emax..."

rm -rf .emax_restore
rm -rf .emacs.d_restore

echo "Going to move aside your current emacs configuration (Don't worry, you can get it back with ./restore.sh"

read -p "Do you want to continue (Y/N)? " -n 1 -r
if [[ ${REPLY} =~ ^[Nn]$ ]]; then
    echo -e "\nNo worries, just run me again if you change your mind. Nothing happens."
    exit 0
fi

echo -e "\n\nOk, here we go..."

if [ -L ~/.emacs.d ]; then
    echo "EMACS_D_SYMLINK_TARGET=`readlink ~/.emacs.d`" >> .emax_restore
    unlink ~/.emacs.d
fi

if [ -e ~/.emacs.d ]; then
    mv ~/.emacs.d .emacs.d_restore
fi

ln -svf "`pwd`/emacs-d" ~/.emacs.d

echo "Great, now restart emacs and you should be sorted!"




