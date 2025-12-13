package com.igormaznitsa.jprol.jsr223;

@FunctionalInterface
public interface JProlScriptEngineProvider {
  JProlScriptEngine getJProlScriptEngine();
}
