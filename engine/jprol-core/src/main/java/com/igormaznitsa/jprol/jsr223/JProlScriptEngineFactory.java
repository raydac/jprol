package com.igormaznitsa.jprol.jsr223;

import com.igormaznitsa.jprol.libs.AbstractJProlLibrary;
import java.util.List;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineFactory;


public class JProlScriptEngineFactory implements ScriptEngineFactory {

  private static final String ENGINE_NAME = "JProl";
  private static final String ENGINE_VERSION = "2.2.2";
  private static final String LANGUAGE_NAME = "Prolog";
  private static final String LANGUAGE_VERSION = "Edinburgh";

  private static final List<String> EXTENSIONS = List.of("pl", "pro", "prolog");
  private static final List<String> MIME = List.of("text/x-prolog", "application/x-prolog");
  private static final List<String> NAMES = List.of("prolog", "Prolog", "jprol", "JProl", "JPROL");

  @Override
  public String getEngineName() {
    return ENGINE_NAME;
  }

  @Override
  public String getEngineVersion() {
    return ENGINE_VERSION;
  }

  @Override
  public List<String> getExtensions() {
    return EXTENSIONS;
  }

  @Override
  public List<String> getMimeTypes() {
    return MIME;
  }

  @Override
  public List<String> getNames() {
    return NAMES;
  }

  @Override
  public String getLanguageName() {
    return LANGUAGE_NAME;
  }

  @Override
  public String getLanguageVersion() {
    return LANGUAGE_VERSION;
  }

  @Override
  public Object getParameter(String key) {
    switch (key) {
      case ScriptEngine.ENGINE:
        return getEngineName();
      case ScriptEngine.ENGINE_VERSION:
        return getEngineVersion();
      case ScriptEngine.LANGUAGE:
        return getLanguageName();
      case ScriptEngine.LANGUAGE_VERSION:
        return getLanguageVersion();
      case ScriptEngine.NAME:
        return getNames().get(0);
      case "THREADING":
        return "MULTITHREADED";
      default:
        return null;
    }
  }

  @Override
  public String getMethodCallSyntax(final String obj, final String method, final String... args) {
    final StringBuilder buffer = new StringBuilder();
    buffer.append(method).append("(").append(obj);
    for (String arg : args) {
      buffer.append(", ").append(arg);
    }
    buffer.append(")");
    return buffer.toString();
  }

  @Override
  public String getOutputStatement(final String toDisplay) {
    return "write('" + toDisplay.replace("'", "\\'") + "'), nl.";
  }

  @Override
  public String getProgram(String... statements) {
    StringBuilder sb = new StringBuilder();
    for (String statement : statements) {
      sb.append(statement);
      if (!statement.trim().endsWith(".")) {
        sb.append(".");
      }
      sb.append("\n");
    }
    return sb.toString();
  }

  @Override
  public ScriptEngine getScriptEngine() {
    return new JProlScriptEngine(this);
  }

  /**
   * Create a script engine with custom libraries.
   *
   * @param libraries JProl library instances to add (in addition to core library)
   * @return A new script engine with the specified libraries
   */
  public ScriptEngine getScriptEngine(AbstractJProlLibrary... libraries) {
    return new JProlScriptEngine(this, libraries);
  }
}
