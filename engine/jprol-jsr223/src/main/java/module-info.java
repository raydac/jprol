open module igormaznitsa.jprol.jcp223 {
  requires transitive igormaznitsa.jprol.core;
  requires transitive java.scripting;

  exports com.igormaznitsa.jprol.jsr223;

  provides javax.script.ScriptEngineFactory with com.igormaznitsa.jprol.jsr223.JProlScriptEngineFactory;
}