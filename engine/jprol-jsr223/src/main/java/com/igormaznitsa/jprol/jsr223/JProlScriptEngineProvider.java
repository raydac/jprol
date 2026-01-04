package com.igormaznitsa.jprol.jsr223;

@FunctionalInterface
public interface JProlScriptEngineProvider extends JProlBindingsConstants {

  JProlScriptEngine getJProlScriptEngine();

}
