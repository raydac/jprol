#!/bin/bash

# Script just generates free desktop descriptor to start application

APP_HOME="$(realpath $(dirname ${BASH_SOURCE[0]}))"
TARGET=$APP_HOME/jprol-editor.desktop

echo [Desktop Entry] > $TARGET
echo Version=1.1 >> $TARGET
echo Encoding=UTF-8 >> $TARGET
echo Type=Application >> $TARGET
echo Name=JProl editor >> $TARGET
echo GenericName=JProl editor >> $TARGET
echo Icon=$APP_HOME/icon.svg >> $TARGET
echo Exec=\"$APP_HOME/run.sh\" %f >> $TARGET
echo Comment=GUI editor for JProl interprter >> $TARGET
echo "Categories=Editor;" >> $TARGET
echo "Keywords=jprol;prolog;editor;" >> $TARGET
echo Terminal=false >> $TARGET
echo StartupWMClass=jprol-editor >> $TARGET

echo Desktop script has been generated: $TARGET

if [ -d ~/.gnome/apps ]; then
    echo copy to ~/.gnome/apps
    cp -f $TARGET ~/.gnome/apps
fi

if [ -d ~/.local/share/applications ]; then
    echo copy to ~/.local/share/applications
    cp -f $TARGET ~/.local/share/applications
fi

if [ -d ~/Desktop ]; then
    echo copy to ~/Desktop
    cp -f $TARGET ~/Desktop
fi

