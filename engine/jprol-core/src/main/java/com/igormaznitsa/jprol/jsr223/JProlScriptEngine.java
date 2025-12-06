package com.igormaznitsa.jprol.jsr223;

import static java.util.Map.entry;
import static java.util.stream.Collectors.toMap;

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermDouble;
import com.igormaznitsa.jprol.data.TermList;
import com.igormaznitsa.jprol.data.TermLong;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.data.TermVar;
import com.igormaznitsa.jprol.data.Terms;
import com.igormaznitsa.jprol.libs.AbstractJProlLibrary;
import com.igormaznitsa.jprol.libs.JProlCoreLibrary;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;
import com.igormaznitsa.jprol.logic.JProlTreeBuilder;
import com.igormaznitsa.jprol.utils.ProlUtils;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.script.AbstractScriptEngine;
import javax.script.Bindings;
import javax.script.Compilable;
import javax.script.CompiledScript;
import javax.script.ScriptContext;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineFactory;
import javax.script.ScriptException;
import javax.script.SimpleBindings;

/**
 * JSR 223 ScriptEngine implementation for JProl Prolog.
 * Allows execution of Prolog code through the standard Java Scripting API.
 * <p>
 * Supports custom libraries via ScriptContext attributes:
 * - "jprol.libraries" - array of JProlLibrary instances
 * - "jprol.context.flags" - Map of initial flags
 *
 * @since 2.2.2
 */
public class JProlScriptEngine extends AbstractScriptEngine implements Compilable {

  private static final JProlCoreLibrary CORE_PROL_LIBRARY = new JProlCoreLibrary();
  private static final Predicate<Term> IS_QUERY_PREDICATE = t -> {
    if (t instanceof TermStruct) {
      final TermStruct struct = (TermStruct) t;
      return struct.getArity() == 1 && "?-".equals(struct.getFunctor().getText());
    }
    return false;
  };
  private final JProlScriptEngineFactory factory;
  private final List<AbstractJProlLibrary> defaultLibraries;
  private JProlContext prologContext;

  JProlScriptEngine(JProlScriptEngineFactory factory) {
    this(factory, new AbstractJProlLibrary[0]);
  }

  JProlScriptEngine(JProlScriptEngineFactory factory, AbstractJProlLibrary... libraries) {
    this.factory = factory;
    this.defaultLibraries = List.of(libraries);
    this.initializeContext(this.defaultLibraries);
  }

  private static Term asTerm(final Object obj) {
    if (obj == null) {
      return Terms.NULL_LIST;
    }
    if (obj instanceof Number) {
      if (obj instanceof Float || obj instanceof Double) {
        return Terms.newDouble(((Number) obj).doubleValue());
      }
      return Terms.newLong(((Number) obj).longValue());
    }
    if (obj instanceof Collection) {
      final List<Term> terms = ((Collection<?>) obj).stream().map(JProlScriptEngine::asTerm)
          .collect(Collectors.toList());
      return TermList.asList(terms);
    }
    return Terms.newAtom(obj.toString());
  }

  private static boolean isPrologVarName(final String string) {
    if (string == null || string.isEmpty()) {
      return false;
    }
    return string.startsWith("_") || Character.isUpperCase(string.charAt(0));
  }

  private void initializeContext(final List<? extends AbstractJProlLibrary> libraries) {
    try {
      final AbstractJProlLibrary[] targetLibraries =
          Stream.concat(Stream.of(CORE_PROL_LIBRARY), libraries.stream()).toArray(
              AbstractJProlLibrary[]::new);
      this.prologContext = new JProlContext(
          "jsr223-context-" + System.identityHashCode(this),
          targetLibraries
      );
    } catch (Exception e) {
      throw new RuntimeException("Failed to initialize JProl context", e);
    }
  }

  @Override
  public Object eval(final String script, final ScriptContext context) throws ScriptException {
    if (script == null) {
      throw new NullPointerException("Script is null");
    }

    try {
      this.checkAndReinitializeWithLibraries(context);
      this.applyContextFlags(context);

      final StringReader reader = new StringReader(script);

      final JProlTreeBuilder treeBuilder = new JProlTreeBuilder(this.prologContext);
      final List<Term> parsedTerms = new ArrayList<>();
      Term nextTerm;
      while ((nextTerm = treeBuilder.readPhraseAndMakeTree(reader)) != null) {
        parsedTerms.add(nextTerm);
      }

      final Bindings bindings = context.getBindings(ScriptContext.ENGINE_SCOPE);

      final Map<String, Term> bindingsAsTerm = bindings == null ? Map.of() :
          bindings.entrySet().stream()
              .filter(x -> isPrologVarName(x.getKey()))
              .map(e -> Map.entry(e.getKey(), asTerm(e.getValue()))).collect(
                  Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));

      final String queryString = parsedTerms.stream().filter(IS_QUERY_PREDICATE).findFirst()
          .map(x -> {
            if (bindingsAsTerm.isEmpty()) {
              return x;
            }
            Term result = x;
            for (final Map.Entry<String, Term> e : bindingsAsTerm.entrySet()) {
              result = result.replaceVar(e.getKey(), e.getValue());
            }
            return result;
          })
          .map(x -> ((TermStruct) x).getElement(0).toSrcString()).orElse(null);
      if (queryString == null) {
        throw new ScriptException("Can't find query predicate '?-' in the query: " + queryString);
      }
      final String consult =
          parsedTerms.stream().filter(Predicate.not(IS_QUERY_PREDICATE)).map(Term::toSrcString)
              .collect(
                  Collectors.joining(". "));
      this.prologContext.consult(new StringReader(consult));
      return this.executeQuery(queryString + '.', context);
    } catch (Exception e) {
      if (e instanceof ScriptException) {
        throw (ScriptException) e;
      }
      throw new ScriptException("Error executing Prolog script: " + e.getMessage());
    }
  }

  @Override
  public Object eval(Reader reader, ScriptContext context) throws ScriptException {
    try {
      StringBuilder sb = new StringBuilder();
      char[] buffer = new char[8192];
      int len;
      while ((len = reader.read(buffer)) != -1) {
        sb.append(buffer, 0, len);
      }
      return eval(sb.toString(), context);
    } catch (IOException e) {
      throw new ScriptException(e);
    }
  }

  @Override
  public Bindings createBindings() {
    return new SimpleBindings();
  }

  @Override
  public ScriptEngineFactory getFactory() {
    return factory;
  }

  @Override
  public CompiledScript compile(String script) throws ScriptException {
    return new JProlCompiledScript(this, script, defaultLibraries);
  }

  @Override
  public CompiledScript compile(Reader reader) throws ScriptException {
    try {
      StringBuilder sb = new StringBuilder();
      char[] buffer = new char[8192];
      int len;
      while ((len = reader.read(buffer)) != -1) {
        sb.append(buffer, 0, len);
      }
      return compile(sb.toString());
    } catch (IOException e) {
      throw new ScriptException(e);
    }
  }

  public List<Map<String, Object>> query(String queryString) throws ScriptException {
    final List<Map<String, Object>> results = new ArrayList<>();
    try {
      final JProlChoicePoint goal = new JProlChoicePoint(queryString, this.prologContext);
      while (goal.prove() != null) {
        results.add(goal.findAllGroundedVars().entrySet().stream()
            .map(x -> entry(x.getKey(), this.convertTermToJava(x.getValue()))).collect(toMap(
                Map.Entry::getKey, Map.Entry::getValue)));
      }
    } catch (Exception e) {
      throw new ScriptException("Error executing query: " + e.getMessage());
    }
    return results;
  }

  public void consult(final String source) throws ScriptException {
    try {
      this.prologContext.consult(new StringReader(source));
    } catch (Exception e) {
      throw new ScriptException("Error consulting Prolog source: " + e.getMessage());
    }
  }

  public JProlContext getPrologContext() {
    return this.prologContext;
  }

  public void resetContext() {
    this.initializeContext(this.defaultLibraries);
  }

  public void addLibraries(final AbstractJProlLibrary... libraries) {
    final List<AbstractJProlLibrary> combined = new ArrayList<>(this.defaultLibraries);
    combined.addAll(List.of(libraries));
    this.initializeContext(combined);
  }

  public void setFlag(final String flagName, final Object value) throws ScriptException {
    try {
      String valueStr = value instanceof String ? "'" + value + "'" : value.toString();
      this.prologContext.consult(
          new StringReader(":-set_prolog_flag(" + flagName + ", " + valueStr + ")."));
    } catch (Exception e) {
      throw new ScriptException("Error setting flag: " + e.getMessage());
    }
  }

  public Object getFlag(String flagName) throws ScriptException {
    try {
      List<Map<String, Object>> results = query("current_prolog_flag(" + flagName + ", Value).");
      if (!results.isEmpty()) {
        return results.get(0).get("Value");
      }
      return null;
    } catch (Exception e) {
      throw new ScriptException("Error getting flag: " + e.getMessage());
    }
  }

  private void checkAndReinitializeWithLibraries(ScriptContext context) {
    final Object libsAttr = context.getAttribute("jprol.libraries", ScriptContext.ENGINE_SCOPE);
    if (libsAttr instanceof Object[]) {
      final List<AbstractJProlLibrary> combined = new ArrayList<>(this.defaultLibraries);
      for (final Object j : (Object[]) libsAttr) {
        if (j instanceof AbstractJProlLibrary) {
          combined.add((AbstractJProlLibrary) j);
        }
      }
      if (!combined.equals(defaultLibraries)) {
        initializeContext(combined);
      }
    }
  }

  private void applyContextFlags(ScriptContext context) throws ScriptException {
    Object flagsAttr = context.getAttribute("jprol.context.flags", ScriptContext.ENGINE_SCOPE);
    if (flagsAttr instanceof Map) {
      @SuppressWarnings("unchecked")
      Map<String, Object> flags = (Map<String, Object>) flagsAttr;
      for (Map.Entry<String, Object> entry : flags.entrySet()) {
        setFlag(entry.getKey(), entry.getValue());
      }
    }
  }

  private Object executeQuery(String queryString, ScriptContext context) throws Exception {
    final JProlChoicePoint goal = new JProlChoicePoint(queryString, this.prologContext);
    final Term result = goal.prove();
    if (result != null) {
      final Bindings bindings = context.getBindings(ScriptContext.ENGINE_SCOPE);
      if (bindings != null) {
        goal.findAllGroundedVars().forEach((n, v) -> {
          bindings.put(n, this.convertTermToJava(v));
        });
      }
      return this.convertTermToJava(result);
    }

    return Boolean.FALSE;
  }

  private Object convertTermToJava(final Term term) {
    if (term == null) {
      return null;
    }

    if (term instanceof TermVar) {
      final Term value = term.findNonVarOrSame();
      if (term == value) {
        return term.getText();
      } else {
        return this.convertTermToJava(value);
      }
    }

    if (term instanceof TermLong) {
      return term.toNumber().longValue();
    }

    if (term instanceof TermDouble) {
      return term.toNumber().doubleValue();
    }

    if (term instanceof TermList) {
      return ProlUtils.listToMappedValues((TermList) term, true, this::convertTermToJava);
    }

    return term.getText();
  }

  private static class JProlCompiledScript extends CompiledScript {
    private final JProlScriptEngine engine;
    private final String script;
    private final List<? extends AbstractJProlLibrary> libraries;
    private final JProlContext compiledContext;

    JProlCompiledScript(JProlScriptEngine engine, String script,
                        List<? extends AbstractJProlLibrary> libraries) throws ScriptException {
      this.engine = engine;
      this.script = script;
      this.libraries = List.copyOf(libraries);

      try {
        this.compiledContext = new JProlContext(
            "compiled-context-" + System.identityHashCode(this),
            Stream.concat(Stream.of(CORE_PROL_LIBRARY), this.libraries.stream()).toArray(
                AbstractJProlLibrary[]::new)
        );
        this.compiledContext.consult(new StringReader(script));
      } catch (Exception e) {
        throw new ScriptException("Error compiling Prolog script: " + e.getMessage());
      }
    }

    @Override
    public Object eval(final ScriptContext context) throws ScriptException {
      final JProlContext oldContext = engine.prologContext;
      try {
        this.engine.prologContext = compiledContext;
        this.engine.applyContextFlags(context);

        String[] lines = script.split("\n");
        Object lastResult = null;

        for (String line : lines) {
          line = line.trim();
          if (line.startsWith("?-")) {
            String query = line.substring(2).trim();
            if (query.endsWith(".")) {
              query = query.substring(0, query.length() - 1).trim();
            }
            lastResult = engine.executeQuery(query, context);
          }
        }

        return lastResult != null ? lastResult : Boolean.TRUE;

      } catch (Exception e) {
        throw new ScriptException("Error evaluating compiled script: " + e.getMessage());
      } finally {
        engine.prologContext = oldContext;
      }
    }

    @Override
    public ScriptEngine getEngine() {
      return this.engine;
    }
  }
}