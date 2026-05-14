package com.igormaznitsa.jprol.jsr223;

/**
 * Marker for objects that expose the owning {@link JProlScriptEngine} (e.g. {@link JProlCompiledScript} and the engine itself).
 */
@FunctionalInterface
public interface JProlScriptEngineProvider extends JProlBindingsConstants {

  /**
   * @return the script engine instance
   */
  JProlScriptEngine getJProlScriptEngine();

}
