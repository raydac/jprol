#!/bin/bash

JPROL_HOME="$(dirname ${BASH_SOURCE[0]})"
LOG_FILE=$JPROL_HOME/console.log
JAVA_HOME=$JPROL_HOME/jre

# uncomment the line below if graphics works slowly
# JAVA_EXTRA_GFX_FLAGS="-Dsun.java2d.opengl=true"

JAVA_FLAGS="-client -XX:+IgnoreUnrecognizedVMOptions --add-opens=java.base/java.lang=ALL-UNNAMED --add-opens=java.base/java.io=ALL-UNNAMED --add-opens=java.base/java.util=ALL-UNNAMED -Dsun.java2d.dpiaware=true -Dswing.aatext=true -Dawt.useSystemAAFontSettings=on"

JAVA_RUN=$JAVA_HOME/bin/java

echo \$JAVA_RUN=$JAVA_RUN &>$LOG_FILE

echo ------JAVA_VERSION------ &>>$LOG_FILE

$JAVA_RUN -version &>>$LOG_FILE

echo ------------------------ &>>$LOG_FILE

$JAVA_RUN $JAVA_FLAGS $JAVA_EXTRA_GFX_FLAGS -jar $JPROL_HOME/jprol-gui.jar $@ &>>$LOG_FILE&
