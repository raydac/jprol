package com.igormaznitsa.jprol.jsr223;

import static java.lang.Double.parseDouble;
import static java.lang.Long.parseLong;

import com.igormaznitsa.jprol.libs.AbstractJProlLibrary;
import com.igormaznitsa.jprol.utils.ProlUtils;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.script.Bindings;
import javax.script.ScriptContext;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineFactory;
import javax.script.SimpleBindings;


public class JProlScriptEngineFactory implements ScriptEngineFactory {

  private static final String ENGINE_NAME = "JProl";
  private static final String ENGINE_VERSION = "3.0.0";
  private static final String LANGUAGE_NAME = "Prolog";
  private static final String LANGUAGE_VERSION = "JProl";

  private static final List<String> EXTENSIONS = List.of("pl", "pro", "prolog");
  private static final List<String> MIME = List.of("text/x-prolog", "application/x-prolog");
  private static final List<String> NAMES =
      List.of("jprol", "JPROL", "JProl", "jprol.prolog", "JProl.Prolog", "JPROL.PROLOG");

  private final Bindings globalBindings = new SimpleBindings(new ConcurrentHashMap<>());

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

  public Bindings getGlobalBindings() {
    return this.globalBindings;
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
    if (obj != null) {
      buffer.append(obj).append(' ');
    }
    buffer.append('\'').append(ProlUtils.escapeSrc(method)).append("'(");
    String comma = "";
    for (final String arg : args) {
      buffer.append(comma);
      comma = ", ";
      try {
        parseLong(arg);
        buffer.append(arg);
      } catch (NumberFormatException ex) {
        try {
          parseDouble(arg);
          buffer.append(arg);
        } catch (NumberFormatException exx) {
          buffer.append('\'').append(ProlUtils.escapeSrc(arg)).append('\'');
        }
      }
    }
    buffer.append(").");
    return buffer.toString();
  }

  @Override
  public String getOutputStatement(final String toDisplay) {
    return "write('" + ProlUtils.escapeSrc(toDisplay) + "'), nl.";
  }

  @Override
  public String getProgram(final String... statements) {
    return Stream.of(statements)
        .filter(Objects::nonNull)
        .map(String::trim)
        .map(x -> {
          if (x.endsWith(".")) {
            return x.substring(0, x.length() - 1);
          } else {
            return x;
          }
        })
        .collect(Collectors.joining(", ", "?- ", "."));
  }

  @Override
  public ScriptEngine getScriptEngine() {
    return this.getScriptEngine(List.of());
  }

  public ScriptEngine getScriptEngine(final AbstractJProlLibrary... libraries) {
    return this.getScriptEngine(
        Arrays.stream(libraries).filter(Objects::nonNull).collect(Collectors.toList()));
  }

  public ScriptEngine getScriptEngine(final List<AbstractJProlLibrary> libraries) {
    final JProlScriptEngine engine = new JProlScriptEngine(this);
    if (libraries != null && !libraries.isEmpty()) {
      engine.getBindings(ScriptContext.ENGINE_SCOPE)
          .put(JProlScriptEngine.JPROL_LIBRARIES, List.copyOf(libraries));
    }
    return engine;
  }

}
