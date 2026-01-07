open module igormaznitsa.jprol.jsr223 {

  exports com.igormaznitsa.jprol.jsr223;

  requires transitive java.scripting;
  requires transitive igormaznitsa.jprol.core;

  provides javax.script.ScriptEngineFactory with com.igormaznitsa.jprol.jsr223.JProlScriptEngineFactory;
}