#!/bin/bash

# Script just generates free desktop descriptor to start application

JPROL_HOME="$(realpath $(dirname ${BASH_SOURCE[0]}))"
TARGET=$JPROL_HOME/jprol.desktop

echo [Desktop Entry] > $TARGET
echo Encoding=UTF-8 >> $TARGET
echo Name=JProl >> $TARGET
echo Comment=Edinburgh Prolog editor >> $TARGET
echo GenericName=JProl editor >> $TARGET
echo Exec=$JPROL_HOME/run.sh >> $TARGET
echo Terminal=false >> $TARGET
echo Type=Application >> $TARGET
echo Icon=$JPROL_HOME/icon.png >> $TARGET
echo Categories=Application; >> $TARGET
echo StartupWMClass=JProl editor >> $TARGET
echo StartupNotify=true >> $TARGET


chmod +x $TARGET

echo Desktop script has been generated: $TARGET
