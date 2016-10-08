#!/bin/bash

JPROL_HOME="$(dirname ${BASH_SOURCE[0]})"
LOG_FILE=$SCIARETO_HOME/console.log

# JAVA_EXTRA_GFX_FLAGS="-Dsun.java2d.opengl=true"

JAVA_FLAGS="-client -Dsun.java2d.dpiaware=true -Dswing.aatext=true -Dawt.useSystemAAFontSettings=on"

if [ -z $JAVA_HOME ]; then
    echo \$JAVA_HOME is undefined &>$LOG_FILE
    JAVA_RUN=java
else
    echo Detected \$JAVA_HOME : $JAVA_HOME &>$LOG_FILE
    JAVA_RUN=$JAVA_HOME/bin/java
fi

if [ -f $JPROL_HOME/.pid ];
then
    SAVED_PID=$(cat $JPROL_HOME/.pid)
    if [ -f /proc/$SAVED_PID/exe ];
    then
        echo Editor already started! if it is wrong, just delete the .pid file in the editor folder root!
	exit 1
    fi
fi    

echo \$JAVA_RUN=$JAVA_RUN &>>$LOG_FILE

$JAVA_RUN $JAVA_FLAGS $JAVA_EXTRA_GFX_FLAGS -Dprol.stack.depth=512m -jar $JPROL_HOME/jprol.jar $@ &>> $JPROL_HOME/console.log&
THE_PID=$!
echo $THE_PID>$JPROL_HOME/.pid
wait $THE_PID
rm $JPROL_HOME/.pid
exit 0

