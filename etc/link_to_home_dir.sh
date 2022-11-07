#!/bin/sh

CONFIG1=${HOME}/.emacs.el
CONFIG2=${HOME}/.emacs
CONFIG3=${HOME}/.emacs.d
FILES=emacs/.emacs.d

if [ -f "$CONFIG1" ]; then
    echo "-------------------------------------------------------------------"
    echo "WARNING: $CONFIG1 PREVENTS LOADING OF EMACS-VIPER CONFIGURATION"
    echo "Remove or rename $CONFIG1 to give EMACS-VIPER a change to load."
    echo "-------------------------------------------------------------------"
    exit 1
fi

if [ -f "$CONFIG2" ]; then
    echo "-------------------------------------------------------------------"
    echo "WARNING: $CONFIG2 PREVENTS LOADING OF EMACS-VIPER CONFIGURATION"
    echo "Remove or rename $CONFIG2 to give EMACS-VIPER a change to load."
    echo "-------------------------------------------------------------------"
    exit 1
fi

if [ -h "$CONFIG3" ]; then
    echo "---------------------------------------------------------------------"
    echo "WARNING: SYMBOLINK LINK $CONFIG3 ALREADY EXISTS ON YOUR SYSTEM, AND  "
    echo "CANNOT BE LINKED TO EMACS-VIPER. PLEASE REMOVE OR RENAME YOUR OLD    "
    echo "CONFIGURATION DIRECTORY TO BE ABLE TO LINK EMACS-VIPER IN ITS PLACE. "
    echo "---------------------------------------------------------------------"
    exit 1
elif [ -d "$CONFIG3" ]; then
    echo "---------------------------------------------------------------------"
    echo "WARNING: DIRECTORY $CONFIG3 ALREADY EXISTS ON YOUR SYSTEM, AND CANNOT"
    echo "BE LINKED TO EMACS-VIPER. PLEASE REMOVE OR RENAME YOUR OLD           "
    echo "CONFIGURATION DIRECTORY TO BE ABLE TO LINK EMACS-VIPER IN ITS PLACE. "
    echo "---------------------------------------------------------------------"
    exit 1
else
    ln -s "${PWD}/${FILES}/" "${CONFIG3}"
fi

