package com.igormaznitsa.jprol.jsr223;

import static com.igormaznitsa.jprol.jsr223.JProlJsr223BootstrapLibrary.READER_IN;
import static com.igormaznitsa.jprol.jsr223.JProlJsr223BootstrapLibrary.WRITER_ERR;
import static com.igormaznitsa.jprol.jsr223.JProlJsr223BootstrapLibrary.WRITER_OUT;
import static java.lang.System.identityHashCode;
import static java.util.Objects.requireNonNull;
import static java.util.stream.Collectors.joining;

import com.igormaznitsa.jprol.data.SourcePosition;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermDouble;
import com.igormaznitsa.jprol.data.TermList;
import com.igormaznitsa.jprol.data.TermLong;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.data.TermVar;
import com.igormaznitsa.jprol.data.Terms;
import com.igormaznitsa.jprol.kbase.KnowledgeBase;
import com.igormaznitsa.jprol.libs.AbstractJProlLibrary;
import com.igormaznitsa.jprol.libs.JProlCoreLibrary;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;
import com.igormaznitsa.jprol.logic.JProlTreeBuilder;
import com.igormaznitsa.jprol.logic.io.IoResourceProvider;
import com.igormaznitsa.jprol.utils.ProlUtils;
import com.igormaznitsa.prologparser.ParserContext;
import com.igormaznitsa.prologparser.exceptions.PrologParserException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.StringReader;
import java.io.Writer;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.locks.ReentrantLock;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.script.Bindings;
import javax.script.Compilable;
import javax.script.CompiledScript;
import javax.script.Invocable;
import javax.script.ScriptContext;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineFactory;
import javax.script.ScriptException;
import javax.script.SimpleBindings;

/**
 * JProl JSR 223 script engine implementation.
 * Allows execution of Prolog code through the standard Java Scripting API.
 * <b>It has MULTIHREADED level, threads not isolated</b>
 * <p>
 * Supports custom libraries via ScriptContext attributes:
 * - "jprol.libraries" - array of JProlLibrary instances
 * - "jprol.context.flags" - Map of initial flags
 *
 * @since 2.2.2
 */
public class JProlScriptEngine
    implements ScriptEngine, Compilable, Invocable, AutoCloseable, JProlScriptEngineProvider {

  static final IoResourceProvider CONSOLE_IO_PROVIDER = new IoResourceProvider() {
    @Override
    public Reader findReader(JProlContext context, String readerId) {
      Reader result = null;
      if (READER_IN.equals(readerId)) {
        result = new InputStreamReader(System.in, Charset.defaultCharset());
      }
      return result;
    }

    @Override
    public Writer findWriter(final JProlContext context, final String writerId,
                             final boolean append) {
      if (WRITER_OUT.equals(writerId)) {
        return new PrintWriter(System.out, true, Charset.defaultCharset());
      } else if (WRITER_ERR.equals(writerId)) {
        return new PrintWriter(System.err, true, Charset.defaultCharset());
      } else {
        return null;
      }
    }
  };

  static final List<AbstractJProlLibrary> BOOTSTRAP_LIBRARIES =
      List.of(new JProlCoreLibrary(), new JProlJsr223BootstrapLibrary());

  static final Function<Term, Term> QUERY_PREDICATE_FILTER = t -> {
    if (t instanceof TermStruct) {
      final TermStruct struct = (TermStruct) t;
      if (struct.getArity() == 1 && "?-".equals(struct.getFunctor().getText())) {
        return struct.getElement(0);
      }
    }
    return null;
  };

  static final Function<Term, Term> NOT_QUERY_PREDICATE_FILTER = t -> {
    if (t instanceof TermStruct) {
      final TermStruct struct = (TermStruct) t;
      if (struct.getArity() == 1 && "?-".equals(struct.getFunctor().getText())) {
        return null;
      }
    }
    return t;
  };
  private static final AbstractJProlLibrary[] EMPTY_LIBRARIES = new AbstractJProlLibrary[0];
  private final JProlScriptEngineFactory factory;
  private final List<AbstractJProlLibrary> defaultLibraries;
  private final AtomicBoolean closed = new AtomicBoolean();
  private final ReentrantLock queryLock = new ReentrantLock();
  private volatile JProlContext prologContext;
  private volatile JProlScriptEngineContext engineContext;

  JProlScriptEngine(JProlScriptEngineFactory factory) {
    this(factory, EMPTY_LIBRARIES);
  }

  JProlScriptEngine(final JProlScriptEngineFactory factory,
                    final AbstractJProlLibrary... libraries) {
    this.engineContext = new JProlScriptEngineContext();
    this.factory = factory;
    this.defaultLibraries = List.of(libraries);
    this.initializeJProlContext(this.defaultLibraries);
  }

  /**
   * Convert a Java object into JProl Term
   *
   * @param obj source object, can be null
   * @return converted object, null will be returned as null list
   */
  static Term java2term(final Object obj) {
    if (obj == null) {
      return Terms.NULL_LIST;
    }
    if (obj instanceof Term) {
      return (Term) obj;
    } else if (obj instanceof Number) {
      if (obj instanceof Float || obj instanceof Double) {
        return Terms.newDouble(((Number) obj).doubleValue());
      }
      return Terms.newLong(((Number) obj).longValue());
    }
    if (obj instanceof Collection) {
      final List<Term> terms = ((Collection<?>) obj).stream().map(JProlScriptEngine::java2term)
          .collect(Collectors.toList());
      return TermList.asList(terms);
    }
    return Terms.newAtom(obj.toString());
  }

  /**
   * Check that a string can be recognized as a prolog variable name.
   *
   * @param string source string, can be null
   * @return true if the string can be recognized as a prolog variable name
   */
  private static boolean isValidPrologVariableName(final String string) {
    if (string == null || string.isEmpty()) {
      return false;
    }
    return string.startsWith("_") || Character.isUpperCase(string.charAt(0));
  }

  /**
   * Search grounded variables in a choice point and fill the result map by converted values.
   *
   * @param choicePoint source choice point, can be null
   * @return a map filled by named java objects found as grounded variables, can't be null but can be empty one
   */
  private static Map<String, Object> extractGroundedVariables(
      final JProlChoicePoint choicePoint) {
    if (choicePoint == null) {
      return Map.of();
    } else {
      return choicePoint.findAllGroundedVars().entrySet().stream().collect(Collectors.toMap(
          Map.Entry::getKey, x -> term2java(x.getValue())));
    }
  }

  private static Object term2java(final Term term) {
    if (term == null) {
      return null;
    }

    if (term instanceof TermVar) {
      final Term value = term.findNonVarOrSame();
      if (term == value) {
        return term.getText();
      } else {
        return term2java(value);
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
          JProlScriptEngine::term2java);
    }

    return term.getText();
  }

  static Map<String, Term> extractVarsFromBindings(final ScriptContext context) {
    final Bindings globalScope = context.getBindings(ScriptContext.GLOBAL_SCOPE);
    final Bindings engineScope = context.getBindings(ScriptContext.ENGINE_SCOPE);

    final Map<String, Term> result = new HashMap<>();
    if (globalScope != null) {
      globalScope.entrySet().stream().filter(x -> isValidPrologVariableName(x.getKey()))
          .forEach(e -> result.put(e.getKey(), java2term(e.getValue())));
    }
    if (engineScope != null) {
      engineScope.entrySet().stream().filter(x -> isValidPrologVariableName(x.getKey()))
          .forEach(e -> result.put(e.getKey(), java2term(e.getValue())));
    }
    return result;
  }

  static List<Term> parseWholeScript(final String script, final JProlContext context) {
    try (final StringReader reader = new StringReader(script)) {
      final JProlTreeBuilder treeBuilder = new JProlTreeBuilder(context);
      final List<Term> parsedTerms = new ArrayList<>();
      Term nextTerm;
      while ((nextTerm = treeBuilder.readPhraseAndMakeTree(reader)) != null) {
        parsedTerms.add(nextTerm);
      }
      return parsedTerms;
    }
  }

  static String joinSources(
      final List<Term> parsed,
      final Function<Term, Term> processor,
      final int limit,
      final Map<String, Term> varReplacements) {
    final String resultString = parsed.stream()
        .map(processor)
        .filter(Objects::nonNull).limit(limit).map(x -> {
          if (varReplacements.isEmpty()) {
            return x;
          }
          Term replaced = x;
          for (final Map.Entry<String, Term> e : varReplacements.entrySet()) {
            replaced = replaced.replaceVar(e.getKey(), e.getValue());
          }
          return replaced;
        })
        .map(Term::toSrcString)
        .collect(joining(". "));
    return resultString.isEmpty() ? resultString : resultString + ".";
  }

  private void assertNotClosed() {
    if (this.closed.get()) {
      throw new IllegalStateException("Already closed");
    }
  }

  @Override
  public JProlScriptEngine getJProlScriptEngine() {
    return this;
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
    return this.eval(script, this.getScriptContext(bindings));
  }

  private ScriptContext getScriptContext(final Bindings bindings) {
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
    this.assertNotClosed();
    return eval(reader, this.getScriptContext(bindings));
  }

  @Override
  public void put(final String key, final Object value) {
    this.assertNotClosed();
    final Bindings bindings = this.getBindings(ScriptContext.ENGINE_SCOPE);
    if (bindings != null) {
      bindings.put(key, value);
    }
  }

  @Override
  public Object get(String key) {
    this.assertNotClosed();
    final Bindings bindings = getBindings(ScriptContext.ENGINE_SCOPE);
    if (bindings != null) {
      return bindings.get(key);
    }
    return null;
  }

  @Override
  public ScriptContext getContext() {
    this.assertNotClosed();
    return this.engineContext;
  }

  @Override
  public void setContext(final ScriptContext context) {
    this.assertNotClosed();
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
          "jprol-jsr223-" + identityHashCode(this),
          targetLibraries
      );
      this.prologContext.addIoResourceProvider(CONSOLE_IO_PROVIDER);
    } catch (Exception e) {
      throw new RuntimeException("Failed to initialize JProl context", e);
    }
  }

  @Override
  public Object eval(final String script, final ScriptContext context) throws ScriptException {
    this.assertNotClosed();

    if (script == null) {
      throw new NullPointerException("Script is null");
    }

    try {
      this.checkAndReinitializeWithLibraries(context);
      this.applyContextFlags(context);

      final List<Term> parsedTerms = parseWholeScript(script, this.prologContext);
      final String queryString =
          joinSources(parsedTerms, QUERY_PREDICATE_FILTER, 1, extractVarsFromBindings(context));
      final String consult =
          joinSources(parsedTerms, NOT_QUERY_PREDICATE_FILTER, Integer.MAX_VALUE, Map.of());
      if (!consult.isBlank()) {
        this.prologContext.consult(new StringReader(consult));
      }
      if (queryString.isBlank()) {
        return Boolean.TRUE;
      }
      return this.executeQuery(queryString, context,
          context.getBindings(ScriptContext.ENGINE_SCOPE));
    } catch (Exception e) {
      if (e instanceof ScriptException) {
        throw (ScriptException) e;
      }
      if (e instanceof PrologParserException) {
        final PrologParserException pe = (PrologParserException) e;
        if (pe.hasValidPosition()) {
          throw new ScriptException(
              "Error parsing Prolog script: " + e.getMessage() + " " + pe.getLine() + ':' +
                  pe.getPos());
        } else {
          throw new ScriptException(
              "Error parsing Prolog script: " + e.getMessage());
        }
      }
      throw new ScriptException("Error executing Prolog script: " + e.getMessage());
    }
  }

  @Override
  public Object eval(final Reader reader, final ScriptContext context) throws ScriptException {
    this.assertNotClosed();
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
    this.assertNotClosed();
    return new SimpleBindings();
  }

  @Override
  public void setBindings(final Bindings bindings, final int scope) {
    this.assertNotClosed();
    this.engineContext.setBindings(bindings, scope);
  }

  @Override
  public ScriptEngineFactory getFactory() {
    this.assertNotClosed();
    return this.factory;
  }

  @Override
  public CompiledScript compile(final String script) throws ScriptException {
    this.assertNotClosed();
    return new JProlCompiledScript(this, script, this.defaultLibraries);
  }

  @Override
  public CompiledScript compile(final Reader reader) throws ScriptException {
    this.assertNotClosed();
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

  public List<Map<String, Object>> query(final String queryString) throws ScriptException {
    this.assertNotClosed();
    final List<Map<String, Object>> results = new ArrayList<>();
    try {
      final JProlChoicePoint goal = new JProlChoicePoint(queryString, this.prologContext);
      while (goal.prove() != null) {
        results.add(extractGroundedVariables(goal));
      }
    } catch (PrologParserException e) {
      if (e.hasValidPosition()) {
        throw new ScriptException(
            "Error parsing query: " + e.getMessage() + " " + e.getLine() + ':' + e.getPos());
      } else {
        throw new ScriptException("Error parsing query: " + e.getMessage());
      }
    } catch (Exception e) {
      throw new ScriptException("Error executing query: " + e.getMessage());
    }
    return results;
  }

  public void consult(final String source) throws ScriptException {
    this.assertNotClosed();
    try {
      this.prologContext.consult(new StringReader(source));
    } catch (Exception e) {
      throw new ScriptException("Error consulting Prolog source: " + e.getMessage());
    }
  }

  public JProlContext getPrologContext() {
    this.assertNotClosed();
    return this.prologContext;
  }

  public void setPrologContext(final JProlContext context) {
    this.assertNotClosed();
    this.prologContext = requireNonNull(context);
  }

  public void resetContext() {
    this.assertNotClosed();
    this.initializeJProlContext(this.defaultLibraries);
  }

  public void addLibraries(final AbstractJProlLibrary... libraries) {
    this.assertNotClosed();
    final List<AbstractJProlLibrary> combined = new ArrayList<>(this.defaultLibraries);
    combined.addAll(List.of(libraries));
    this.initializeJProlContext(combined);
  }

  public void setFlag(final String flagName, final Object value) throws ScriptException {
    this.assertNotClosed();
    try {
      String valueStr = value instanceof String ? "'" + value + "'" : value.toString();
      this.prologContext.consult(
          new StringReader(":- set_prolog_flag(" + flagName + ", " + valueStr + ")."));
    } catch (PrologParserException e) {
      if (e.hasValidPosition()) {
        throw new ScriptException(
            "Error parsing query: " + e.getMessage() + " " + e.getLine() + ':' + e.getPos());
      } else {
        throw new ScriptException("Error parsing query: " + e.getMessage());
      }
    } catch (Exception e) {
      throw new ScriptException("Error setting flag: " + e.getMessage());
    }
  }

  public Object getFlag(final String flagName) throws ScriptException {
    this.assertNotClosed();
    try {
      final List<Map<String, Object>> results =
          this.query("current_prolog_flag(" + flagName + ", Value).");
      if (!results.isEmpty()) {
        return results.get(0).get("Value");
      }
      return null;
    } catch (Exception e) {
      if (e instanceof ScriptException) {
        throw (ScriptException) e;
      } else {
        throw new ScriptException("Error getting flag: " + e.getMessage());
      }
    }
  }

  void checkAndReinitializeWithLibraries(final ScriptContext context) {
    this.assertNotClosed();
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

  void applyContextFlags(final ScriptContext context) throws ScriptException {
    this.assertNotClosed();
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
  public Bindings getBindings(final int scope) {
    this.assertNotClosed();
    switch (scope) {
      case ScriptContext.ENGINE_SCOPE:
        return this.engineContext.getBindings(ScriptContext.ENGINE_SCOPE);
      case ScriptContext.GLOBAL_SCOPE:
        return this.engineContext.getBindings(ScriptContext.GLOBAL_SCOPE);
      default:
        throw new IllegalArgumentException("Invalid scope value.");
    }
  }

  Object executeQuery(final String queryString, final ScriptContext context,
                      final Bindings bindings) {
    this.assertNotClosed();
    this.queryLock.lock();
    try {
      final JProlChoicePoint goal = new JProlChoicePoint(queryString, this.prologContext);
      final Term result = goal.prove();
      if (result != null) {
        final Map<String, Object> groundedVars = extractGroundedVariables(goal);
        if (bindings != null) {
          bindings.putAll(groundedVars);
        }
        return Boolean.TRUE;
      }
      return Boolean.FALSE;
    } finally {
      this.queryLock.unlock();
    }
  }

  Object executeQuery(final Term queryTerm, final ScriptContext context, final Bindings bindings) {
    this.assertNotClosed();
    this.queryLock.lock();
    try {
      final JProlChoicePoint goal = new JProlChoicePoint(queryTerm, this.prologContext);
      final Term result = goal.prove();
      if (result != null) {
        final Map<String, Object> groundedVars = extractGroundedVariables(goal);
        if (bindings != null) {
          bindings.putAll(groundedVars);
        }
        return Boolean.TRUE;
      }
      return Boolean.FALSE;
    } finally {
      this.queryLock.unlock();
    }
  }

  @Override
  public Object invokeMethod(final Object thisObject, final String name, final Object... args)
      throws ScriptException {
    return this.invokeFunction(name, args);
  }

  @Override
  public Object invokeFunction(final String name, final Object... args)
      throws ScriptException {
    this.assertNotClosed();
    final Term[] terms = new Term[args.length];
    for (int i = 0; i < args.length; i++) {
      terms[i] = java2term(args[i]);
    }
    final Term term = Terms.newStruct(name, terms, SourcePosition.UNKNOWN);

    this.checkAndReinitializeWithLibraries(this.engineContext);
    this.applyContextFlags(this.engineContext);

    return this.executeQuery(term, this.engineContext,
        this.engineContext.getBindings(ScriptContext.ENGINE_SCOPE));
  }

  @Override
  public <T> T getInterface(final Class<T> targetClass) {
    return this.getInterface(this, targetClass);
  }

  @SuppressWarnings("unchecked")
  @Override
  public <T> T getInterface(final Object thisObject, final Class<T> targetClass) {
    this.assertNotClosed();

    if (thisObject instanceof JProlScriptEngineProvider) {
      if (targetClass == null) {
        throw new NullPointerException("Class must not be null");
      }

      final JProlScriptEngine engine =
          ((JProlScriptEngineProvider) thisObject).getJProlScriptEngine();

      final T result;
      if (targetClass.isAssignableFrom(JProlScriptEngine.class)) {
        result = (T) engine;
      } else {
        final JProlContext prolContext = engine.prologContext;
        if (prolContext != null && targetClass.isAssignableFrom(JProlContext.class)) {
          result = (T) prolContext;
        } else if (prolContext != null && targetClass.isAssignableFrom(ParserContext.class)) {
          result = (T) prolContext.getParserContext();
        } else if (prolContext != null && targetClass.isAssignableFrom(KnowledgeBase.class)) {
          result = (T) prolContext.getKnowledgeBase();
        } else {
          result = null;
        }
      }
      return result;
    } else {
      throw new IllegalArgumentException(
          "Expected " + JProlScriptEngineProvider.class.getCanonicalName() + " instance");
    }
  }

  @Override
  public void close() {
    if (this.closed.compareAndSet(false, true)) {
      final JProlContext currentContext = this.prologContext;
      this.prologContext = null;
      if (currentContext != null) {
        currentContext.close();
      }
      final JProlScriptEngineContext thisEngineContext = this.engineContext;
      this.engineContext = null;
      if (thisEngineContext != null) {
        thisEngineContext.close();
      }
    }
  }
}