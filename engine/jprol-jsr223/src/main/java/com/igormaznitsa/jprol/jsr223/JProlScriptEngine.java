package com.igormaznitsa.jprol.jsr223;

import static com.igormaznitsa.jprol.jsr223.JProlJsr223BootstrapLibrary.READER_IN;
import static com.igormaznitsa.jprol.jsr223.JProlJsr223BootstrapLibrary.WRITER_ERR;
import static com.igormaznitsa.jprol.jsr223.JProlJsr223BootstrapLibrary.WRITER_OUT;

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
import com.igormaznitsa.jprol.logic.io.IoResourceProvider;
import com.igormaznitsa.jprol.utils.ProlUtils;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.StringReader;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;
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
public class JProlScriptEngine implements ScriptEngine, Compilable {

  private static final IoResourceProvider CONSOLE_IO_PROVIDER = new IoResourceProvider() {
    @Override
    public Reader findReader(JProlContext context, String readerId) {
      Reader result = null;
      if (READER_IN.equals(readerId)) {
        result = new InputStreamReader(System.in);
      }
      return result;
    }

    @Override
    public Writer findWriter(JProlContext context, String writerId, boolean append) {
      if (WRITER_OUT.equals(writerId)) {
        return new PrintWriter(System.out);
      } else if (WRITER_ERR.equals(writerId)) {
        return new PrintWriter(System.err);
      } else {
        return null;
      }
    }
  };

  private static final List<AbstractJProlLibrary> BOOTSTRAP_LIBRARIES =
      List.of(new JProlCoreLibrary(), new JProlJsr223BootstrapLibrary());
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

  private volatile JProlScriptEngineContext engineContext;

  JProlScriptEngine(JProlScriptEngineFactory factory) {
    this(factory, new AbstractJProlLibrary[0]);
  }

  JProlScriptEngine(JProlScriptEngineFactory factory, AbstractJProlLibrary... libraries) {
    this.engineContext = new JProlScriptEngineContext();
    this.factory = factory;
    this.defaultLibraries = List.of(libraries);
    this.initializeJProlContext(this.defaultLibraries);
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

  private static Map<String, Object> extractAllGroundedVariables(
      final JProlChoicePoint choicePoint) {
    if (choicePoint == null) {
      return Map.of();
    } else {
      return choicePoint.findAllGroundedVars().entrySet().stream().collect(Collectors.toMap(
          Map.Entry::getKey, x -> convertTermToJava(x.getValue())));
    }
  }

  private static Object convertTermToJava(final Term term) {
    if (term == null) {
      return null;
    }

    if (term instanceof TermVar) {
      final Term value = term.findNonVarOrSame();
      if (term == value) {
        return term.getText();
      } else {
        return convertTermToJava(value);
      }
    }

    if (term instanceof TermLong) {
      return term.toNumber().longValue();
    }

    if (term instanceof TermDouble) {
      return term.toNumber().doubleValue();
    }

    if (term instanceof TermList) {
      return ProlUtils.listToMappedValues((TermList) term, true,
          JProlScriptEngine::convertTermToJava);
    }

    return term.getText();
  }

  private static Map<String, Term> findVarsInBindings(final ScriptContext context) {
    final Bindings globalScope = context.getBindings(ScriptContext.GLOBAL_SCOPE);
    final Bindings engineScope = context.getBindings(ScriptContext.ENGINE_SCOPE);

    final Map<String, Term> result = new HashMap<>();
    globalScope.entrySet().stream().filter(x -> isPrologVarName(x.getKey()))
        .forEach(e -> result.put(e.getKey(), asTerm(e.getValue())));
    engineScope.entrySet().stream().filter(x -> isPrologVarName(x.getKey()))
        .forEach(e -> result.put(e.getKey(), asTerm(e.getValue())));
    return result;
  }

  @Override
  public Object eval(String script) throws ScriptException {
    return this.eval(script, this.engineContext);
  }

  @Override
  public Object eval(Reader reader) throws ScriptException {
    return this.eval(reader, this.engineContext);
  }

  @Override
  public Object eval(String script, Bindings bindings) throws ScriptException {
    return eval(script, this.getScriptContext(bindings));
  }

  protected ScriptContext getScriptContext(Bindings bindings) {
    final JProlScriptEngineContext newContext = new JProlScriptEngineContext(
        this.engineContext.getReader(), this.engineContext.getWriter(),
        this.engineContext.getErrorWriter());
    Bindings globalScope = this.engineContext.getBindings(ScriptContext.GLOBAL_SCOPE);
    if (globalScope != null) {
      newContext.setBindings(globalScope, ScriptContext.GLOBAL_SCOPE);
    }

    if (bindings != null) {
      newContext.setBindings(bindings,
          ScriptContext.ENGINE_SCOPE);
    } else {
      throw new NullPointerException("Engine scope Bindings may not be null.");
    }

    return newContext;
  }

  @Override
  public Object eval(final Reader reader, final Bindings bindings) throws ScriptException {
    return eval(reader, this.getScriptContext(bindings));
  }

  @Override
  public void put(final String key, final Object value) {
    final Bindings bindings = this.getBindings(ScriptContext.ENGINE_SCOPE);
    if (bindings != null) {
      bindings.put(key, value);
    }
  }

  @Override
  public Object get(String key) {
    final Bindings bindings = getBindings(ScriptContext.ENGINE_SCOPE);
    if (bindings != null) {
      return bindings.get(key);
    }
    return null;
  }

  @Override
  public ScriptContext getContext() {
    return this.engineContext;
  }

  @Override
  public void setContext(final ScriptContext context) {
    if (context instanceof JProlScriptEngineContext) {
      this.engineContext = (JProlScriptEngineContext) context;
    } else {
      throw new IllegalArgumentException(
          "Expected " + JProlScriptEngineContext.class.getCanonicalName());
    }
  }

  private void initializeJProlContext(final List<? extends AbstractJProlLibrary> libraries) {
    try {
      final AbstractJProlLibrary[] targetLibraries =
          Stream.concat(BOOTSTRAP_LIBRARIES.stream(), libraries.stream()).toArray(
              AbstractJProlLibrary[]::new);
      this.prologContext = new JProlContext(
          "jsr223-context-" + System.identityHashCode(this),
          targetLibraries
      );
      this.prologContext.addIoResourceProvider(CONSOLE_IO_PROVIDER);
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

      final Map<String, Term> bindingsAsTerm = findVarsInBindings(context);
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
        throw new ScriptException("Can't find query predicate '?-' in the script: " + script);
      }
      final String consult =
          parsedTerms.stream().filter(Predicate.not(IS_QUERY_PREDICATE)).map(Term::toSrcString)
              .collect(
                  Collectors.joining(" ", "", "."));
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
  public void setBindings(Bindings bindings, int scope) {
    this.engineContext.setBindings(bindings, scope);
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
        results.add(extractAllGroundedVariables(goal));
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
    this.initializeJProlContext(this.defaultLibraries);
  }

  public void addLibraries(final AbstractJProlLibrary... libraries) {
    final List<AbstractJProlLibrary> combined = new ArrayList<>(this.defaultLibraries);
    combined.addAll(List.of(libraries));
    this.initializeJProlContext(combined);
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
      List<Map<String, Object>> results =
          this.query("current_prolog_flag(" + flagName + ", Value).");
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
        initializeJProlContext(combined);
      }
    }
  }

  private void applyContextFlags(ScriptContext context) throws ScriptException {
    Object flagsAttr = context.getAttribute("jprol.context.flags", ScriptContext.ENGINE_SCOPE);
    if (flagsAttr instanceof Map) {
      @SuppressWarnings("unchecked") final Map<String, Object> flags =
          (Map<String, Object>) flagsAttr;
      for (Map.Entry<String, Object> entry : flags.entrySet()) {
        this.setFlag(entry.getKey(), entry.getValue());
      }
    }
  }

  @Override
  public Bindings getBindings(int scope) {
    switch (scope) {
      case ScriptContext.ENGINE_SCOPE:
        return this.engineContext.getBindings(ScriptContext.ENGINE_SCOPE);
      case ScriptContext.GLOBAL_SCOPE:
        return this.engineContext.getBindings(ScriptContext.GLOBAL_SCOPE);
      default:
        throw new IllegalArgumentException("Invalid scope value.");
    }
  }

  private Object executeQuery(final String queryString, final ScriptContext context) {
    final JProlChoicePoint goal = new JProlChoicePoint(queryString, this.prologContext);
    final Term result = goal.prove();
    if (result != null) {
      final Map<String, Object> groundedVars = extractAllGroundedVariables(goal);
      final Bindings engineBindings = context.getBindings(ScriptContext.ENGINE_SCOPE);
      if (engineBindings != null) {
        engineBindings.putAll(groundedVars);
      }
      return Boolean.TRUE;
    }
    return Boolean.FALSE;
  }

  private static class JProlCompiledScript extends CompiledScript {
    private final JProlScriptEngine engine;
    private final String script;
    private final JProlContext compiledContext;

    JProlCompiledScript(JProlScriptEngine engine, String script,
                        List<? extends AbstractJProlLibrary> libraries) throws ScriptException {
      this.engine = engine;
      this.script = script;
      final List<? extends AbstractJProlLibrary> libraries1 = List.copyOf(libraries);

      try {
        this.compiledContext = new JProlContext(
            "compiled-context-" + System.identityHashCode(this),
            Stream.concat(BOOTSTRAP_LIBRARIES.stream(), libraries1.stream()).toArray(
                AbstractJProlLibrary[]::new)
        );
        this.compiledContext.addIoResourceProvider(CONSOLE_IO_PROVIDER);
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