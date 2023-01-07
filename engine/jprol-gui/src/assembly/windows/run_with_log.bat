set "JPROL_HOME=%cd%"
set "LOG_FILE=%JPROL_HOME%/console.log"

rem uncomment the line below if graphics works slowly
rem set "JAVA_EXTRA_GFX_FLAGS=-Dsun.java2d.opengl=true"

set "JAVA_FLAGS=-client -Xss25M -Xmx2G"
set "JAVA_RUN=java.exe"

echo %%JAVA_RUN%%=%JAVA_RUN% > %LOG_FILE%

echo ------JAVA_VERSION------ >> %LOG_FILE%

%JAVA_RUN% -version 2>> %LOG_FILE%

echo ------------------------ >> %LOG_FILE%

%JPROL_HOME%\jre\bin\%JAVA_RUN% %JAVA_FLAGS% %JAVA_EXTRA_GFX_FLAGS% -jar %JPROL_HOME%\jprol-gui.jar %* 2>> %LOG_FILE%
