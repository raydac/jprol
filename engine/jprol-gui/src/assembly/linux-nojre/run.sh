#!/bin/bash

JPROL_HOME="$(dirname ${BASH_SOURCE[0]})"
JAVA_HOME=$JPROL_HOME/jre

# uncomment the line below if graphics works slowly
# JAVA_EXTRA_GFX_FLAGS="-Dsun.java2d.opengl=true"

JAVA_FLAGS="-client -Xss25M -Xmx2G"
JAVA_LOG_CONFIG=$JPROL_HOME/logger.properties
JAVA_RUN=$JAVA_HOME/bin/java

$JAVA_RUN $JAVA_FLAGS $JAVA_EXTRA_GFX_FLAGS "-Djava.util.logging.config.file=$JAVA_LOG_CONFIG" -jar $JPROL_HOME/jprol-gui.jar $@
