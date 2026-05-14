package com.igormaznitsa.jprol.jsr223;

import static com.igormaznitsa.jprol.data.Terms.newAtom;
import static com.igormaznitsa.jprol.jsr223.JProlJsr223BootstrapLibrary.READER_USER;
import static com.igormaznitsa.jprol.jsr223.JProlJsr223BootstrapLibrary.WRITER_ERR;
import static com.igormaznitsa.jprol.jsr223.JProlJsr223BootstrapLibrary.WRITER_USER;
import static com.igormaznitsa.jprol.jsr223.JProlScriptEngineUtils.asJProlContext;
import static com.igormaznitsa.jprol.jsr223.JProlScriptEngineUtils.isValidPrologVariableName;
import static com.igormaznitsa.jprol.jsr223.JProlScriptEngineUtils.java2term;
import static com.igormaznitsa.jprol.jsr223.JProlScriptEngineUtils.term2java;
import static java.util.Objects.requireNonNull;
import static java.util.stream.Collectors.joining;

import com.igormaznitsa.jprol.data.SourcePosition;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.data.Terms;
import com.igormaznitsa.jprol.kbase.KnowledgeBase;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;
import com.igormaznitsa.jprol.logic.JProlSystemFlag;
import com.igormaznitsa.jprol.logic.JProlTreeBuilder;
import com.igormaznitsa.jprol.logic.io.IoResourceProvider;
import com.igormaznitsa.jprol.utils.lazy.LazyMap;
import com.igormaznitsa.prologparser.ParserContext;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.StringReader;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;
import java.util.stream.Collectors;
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
 * JProl {@link ScriptEngine} for JSR 223 ({@code javax.script}): consult and query Prolog text from Java.
 * <p>
 * <b>Threading:</b> {@link ScriptEngineFactory#getParameter} with key {@code "THREADING"} returns {@code "MULTITHREADED"}.
 * {@link JProlContext} is stored per {@linkplain Thread thread} inside {@link JProlScriptEngineContext}; threads are not isolated.
 * <p>
 * <b>Evaluation ({@link #eval(String)}, {@link #eval(Reader, ScriptContext)}, etc.):</b> The script is parsed as a sequence of
 * Prolog phrases. All non-query phrases are {@linkplain JProlContext#consult consulted} into the {@link JProlContext} taken from
 * the {@link ScriptContext} passed to {@code eval} (or from {@link #getContext()} for overloads without a context argument).
 * The first {@code ?- Goal.} form is run once (first solution only). Grounded query variables are written into the target
 * {@link Bindings}; only binding keys that are valid Prolog variable names are mapped (see {@link JProlScriptEngineUtils#isValidPrologVariableName(String)}).
 * If the script contains no query, {@link Boolean#TRUE} is returned.
 * <p>
 * <b>Bindings / attributes</b> (see {@link JProlBindingsConstants} for keys):
 * <ul>
 *   <li>{@value JProlBindingsConstants#JPROL_LIBRARIES} &mdash; extra {@link com.igormaznitsa.jprol.libs.AbstractJProlLibrary} instances when a context is created</li>
 *   <li>{@value JProlBindingsConstants#JPROL_CONTEXT_FLAGS} &mdash; {@link Map}{@code <String,Object>} of JProl system flags at init</li>
 *   <li>{@value JProlBindingsConstants#JPROL_GLOBAL_EXECUTOR_SERVICE}, {@value JProlBindingsConstants#JPROL_GLOBAL_KNOWLEDGE_BASE},
 *       {@value JProlBindingsConstants#JPROL_GLOBAL_GUARD_PREDICATE} &mdash; optional global scope only</li>
 * </ul>
 *
 * @see JProlBindingsConstants
 * @since 3.0.0
 */
public class JProlScriptEngine
    implements Disposable, CanGC, ScriptEngine, Compilable, Invocable, JProlScriptEngineProvider,
    AutoCloseable {

  static final IoResourceProvider CONSOLE_IO_PROVIDER = new IoResourceProvider() {
    @Override
    public Reader findReader(JProlContext context, String readerId) {
      Reader result = null;
      if (READER_USER.equals(readerId)) {
        result = new InputStreamReader(System.in, StandardCharsets.UTF_8);
      }
      return result;
    }

    @Override
    public Writer findWriter(final JProlContext context, final String writerId,
                             final boolean append) {
      if (WRITER_USER.equals(writerId)) {
        return new PrintWriter(System.out, true, StandardCharsets.UTF_8);
      } else if (WRITER_ERR.equals(writerId)) {
        return new PrintWriter(System.err, true, StandardCharsets.UTF_8);
      } else {
        return null;
      }
    }
  };
  static final Function<Term, Term> QUERY_PREDICATE_FILTER = t -> {
    if (t instanceof TermStruct) {
      final TermStruct struct = (TermStruct) t;
      if (struct.getArity() == 1 && "?-".equals(struct.getFunctor().getText())) {
        return struct.getArgumentAt(0);
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
  private final JProlScriptEngineFactory factory;
  private final AtomicBoolean disposed = new AtomicBoolean();
  private final AtomicReference<JProlScriptEngineContext> engineContext = new AtomicReference<>();

  /**
   * Creates an engine bound to the given factory; initial {@link ScriptContext} is a new {@link JProlScriptEngineContext}.
   */
  JProlScriptEngine(final JProlScriptEngineFactory factory) {
    this.factory = factory;
    final JProlScriptEngineContext newContext = new JProlScriptEngineContext(this.factory);
    this.engineContext.set(newContext);
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

  static Map<String, Term> extractVarsFromBindings(final ScriptContext context,
                                                   final Bindings customEngineBindings) {
    final Bindings globalScope = context.getBindings(ScriptContext.GLOBAL_SCOPE);
    final Bindings engineScope =
        customEngineBindings == null ? context.getBindings(ScriptContext.ENGINE_SCOPE) :
            customEngineBindings;

    final Map<String, Term> result = new LazyMap<>();
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

  static List<Term> parseWholeScript(final Reader script, final JProlContext context) {
    try (
        final JProlTreeBuilder treeBuilder = new JProlTreeBuilder(context, script,
            false)) {
      final List<Term> parsedTerms = new ArrayList<>();
      Term nextTerm;
      while ((nextTerm = treeBuilder.readPhraseAndMakeTree()) != null) {
        parsedTerms.add(nextTerm);
      }
      return parsedTerms;
    }
  }

  static List<Term> parseWholeScript(final String script, final JProlContext context) {
    try (
        final JProlTreeBuilder treeBuilder = new JProlTreeBuilder(context, new StringReader(script),
            false)) {
      final List<Term> parsedTerms = new ArrayList<>();
      Term nextTerm;
      while ((nextTerm = treeBuilder.readPhraseAndMakeTree()) != null) {
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

  /**
   * Disposing existing thread local JProl context and create new one from current context parameters.
   * Knowledge base can be affected.
   */
  public void reinitJProlContext() {
    this.assertNotClosed();
    this.engineContext.get().reinitJProlContext();
  }

  private void assertNotClosed() {
    if (this.disposed.get()) {
      throw new IllegalStateException("Already closed");
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public JProlScriptEngine getJProlScriptEngine() {
    return this;
  }

  /**
   * {@inheritDoc}
   * <p>Evaluates using {@link #getContext()} as the {@link ScriptContext}.
   */
  @Override
  public Object eval(String script) throws ScriptException {
    return this.eval(script, this.engineContext.get());
  }

  /**
   * {@inheritDoc}
   * <p>Evaluates using {@link #getContext()} as the {@link ScriptContext}.
   */
  @Override
  public Object eval(final Reader reader) throws ScriptException {
    return this.eval(reader, this.engineContext.get());
  }

  /**
   * {@inheritDoc}
   * <p>Evaluates using {@link #getContext()}; variable bindings are taken from {@code bindings} for the query only.
   */
  @Override
  public Object eval(final String script, final Bindings bindings) throws ScriptException {
    return this.eval(new StringReader(script), this.engineContext.get(), bindings);
  }

  /**
   * {@inheritDoc}
   * <p>Evaluates using {@link #getContext()}; variable bindings are taken from {@code bindings} for the query only.
   */
  @Override
  public Object eval(final Reader reader, final Bindings bindings) throws ScriptException {
    this.assertNotClosed();
    return this.eval(reader, this.engineContext.get(), bindings);
  }

  /**
   * {@inheritDoc}
   * <p>Delegates to {@link ScriptContext#ENGINE_SCOPE} bindings of this engine.
   */
  @Override
  public void put(final String key, final Object value) {
    this.assertNotClosed();
    final Bindings bindings = this.getBindings(ScriptContext.ENGINE_SCOPE);
    if (bindings != null) {
      bindings.put(key, value);
    }
  }

  /**
   * {@inheritDoc}
   * <p>Reads from {@link ScriptContext#ENGINE_SCOPE} bindings of this engine.
   */
  @Override
  public Object get(final String key) {
    this.assertNotClosed();
    final Bindings bindings = this.getBindings(ScriptContext.ENGINE_SCOPE);
    if (bindings != null) {
      return bindings.get(key);
    }
    return null;
  }

  /**
   * {@inheritDoc}
   * <p>Returns this engine's {@link JProlScriptEngineContext} (the concrete {@link ScriptContext} implementation).
   */
  @Override
  public ScriptContext getContext() {
    this.assertNotClosed();
    return this.engineContext.get();
  }

  /**
   * Replaces the engine {@link ScriptContext}. The argument must be a {@link JProlScriptEngineContext}.
   * The previous context is {@linkplain JProlScriptEngineContext#gc() gc}'d (not disposed).
   *
   * @throws IllegalArgumentException if {@code context} is not a {@link JProlScriptEngineContext}
   */
  @Override
  public void setContext(final ScriptContext context) {
    this.assertNotClosed();
    final JProlScriptEngineContext prev =
        this.engineContext.getAndSet(asJProlContext(requireNonNull(context)));
    if (prev != null) {
      prev.gc();
    }
  }

  /**
   * Find {@code ?-} query in the script, consult the rest, prove the query once, and write grounded variables into bindings.
   * <p>The {@code context} argument must be a {@link JProlScriptEngineContext}; its thread-local {@link JProlContext} is used
   * for both consult and the query (see class documentation).
   *
   * @param script  prolog script
   * @param context target context (must be {@link JProlScriptEngineContext})
   * @return {@link Boolean#TRUE} if proved, {@link Boolean#FALSE} otherwise; {@link Boolean#TRUE} if there is no query phrase
   * @throws ScriptException if any internal error
   */
  @Override
  public Object eval(final String script, final ScriptContext context) throws ScriptException {
    return this.eval(new StringReader(script), context, null);
  }

  /**
   * Find {@code ?-} query in the script, consult the rest, prove the query once. Variable values for the query are taken
   * from {@code context} bindings (and optional {@code customEngineBindings} instead of engine-scope bindings for mapping).
   * <p>{@code context} must be a {@link JProlScriptEngineContext}; consult and prove use the same thread-local {@link JProlContext}.
   *
   * @param script               prolog script
   * @param context              target context (must be {@link JProlScriptEngineContext})
   * @param customEngineBindings if non-null, used with {@code context} for Prolog variable substitution instead of engine-scope bindings only
   * @return {@link Boolean#TRUE} if proved, {@link Boolean#FALSE} otherwise; {@link Boolean#TRUE} if there is no query phrase
   * @throws ScriptException if any internal error
   */
  public Object eval(
      final Reader script,
      final ScriptContext context,
      final Bindings customEngineBindings
  ) throws ScriptException {
    this.assertNotClosed();

    if (script == null) {
      throw new NullPointerException("Script is null");
    }

    try {
      final JProlContext prolContext =
          asJProlContext(requireNonNull(context, "context must not be null"))
              .findOrMakeJProlContext();

      final List<Term> parsedTerms = parseWholeScript(script, prolContext);
      final String queryString =
          joinSources(parsedTerms, QUERY_PREDICATE_FILTER, 1,
              extractVarsFromBindings(context, customEngineBindings));
      final String consult =
          joinSources(parsedTerms, NOT_QUERY_PREDICATE_FILTER, Integer.MAX_VALUE, Map.of());
      if (!consult.isBlank()) {
        prolContext.consult(new StringReader(consult));
      }
      if (queryString.isBlank()) {
        return Boolean.TRUE;
      }
      return this.proveQueryOnce(queryString, context,
          customEngineBindings == null ? context.getBindings(ScriptContext.ENGINE_SCOPE) :
              customEngineBindings);
    } catch (Exception e) {
      throw new ScriptException(e);
    }
  }

  /**
   * Same as {@link #eval(String, ScriptContext)} but reads script from a {@link Reader}; {@code context} must be a
   * {@link JProlScriptEngineContext}.
   */
  @Override
  public Object eval(final Reader reader, final ScriptContext context) throws ScriptException {
    return this.eval(reader, context, null);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Bindings createBindings() {
    this.assertNotClosed();
    return new SimpleBindings();
  }

  /**
   * {@inheritDoc}
   * <p>Only {@link ScriptContext#GLOBAL_SCOPE} and {@link ScriptContext#ENGINE_SCOPE} are supported.
   */
  @Override
  public void setBindings(final Bindings bindings, final int scope) {
    this.assertNotClosed();
    this.engineContext.get().setBindings(bindings, scope);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ScriptEngineFactory getFactory() {
    this.assertNotClosed();
    return this.factory;
  }

  /**
   * {@inheritDoc}
   * <p>Produces a {@link JProlCompiledScript} bound to the {@link JProlContext} of the compiling thread at compile time.
   */
  @Override
  public CompiledScript compile(final String script) throws ScriptException {
    this.assertNotClosed();
    return new JProlCompiledScript(this, script);
  }

  /**
   * {@inheritDoc}
   * <p>Reads the reader fully into a string, then {@linkplain #compile(String) compiles} it.
   */
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

  /**
   * Prove goal until it fails and collect all results as a list.
   *
   * @param queryGoal a query to be proven, must not be null
   * @return list of all results collected during prove
   * @throws ScriptException thrown if any problem
   */
  public List<Map<String, Object>> proveAll(final Term queryGoal) throws ScriptException {
    this.assertNotClosed();
    final JProlContext prolContext = this.engineContext.get().findOrMakeJProlContext();
    return this.findAllResults(prolContext.makeChoicePoint(queryGoal));
  }


  /**
   * Prove a query goal by raw string (without '?-') until it fails and collect all results as a list.
   *
   * @param queryGoal a query to be proven, must not be null
   * @return list of all results collected during prove
   * @throws ScriptException thrown if any problem
   */
  public List<Map<String, Object>> proveAll(final String queryGoal) throws ScriptException {
    this.assertNotClosed();
    final JProlContext prolContext = this.engineContext.get().findOrMakeJProlContext();
    return this.findAllResults(prolContext.makeChoicePoint(queryGoal));
  }

  private List<Map<String, Object>> findAllResults(final JProlChoicePoint goalCp)
      throws ScriptException {
    this.assertNotClosed();
    final List<Map<String, Object>> results = new ArrayList<>();
    try {
      while (goalCp.prove() != null) {
        results.add(extractGroundedVariables(goalCp));
      }
    } catch (Exception e) {
      throw new ScriptException(e);
    }
    return results;
  }

  /**
   * Make consult for engine context.
   *
   * @param source sources
   * @throws ScriptException if any error
   */
  public void consult(final String source) throws ScriptException {
    this.assertNotClosed();
    try {
      this.engineContext.get().findOrMakeJProlContext().consult(new StringReader(source));
    } catch (Exception e) {
      throw new ScriptException(e);
    }
  }

  /**
   * Reads the current value of a JProl {@linkplain JProlSystemFlag system flag} as a Java object.
   *
   * @param flagName atom name of the flag (see {@link JProlSystemFlag})
   * @return flag value converted for Java use
   * @throws IllegalArgumentException if the name is not a known flag
   * @throws ScriptException          on internal errors
   */
  public Object getFlag(final String flagName) throws ScriptException {
    this.assertNotClosed();
    final JProlSystemFlag flag = JProlSystemFlag.find(newAtom(flagName))
        .orElseThrow(() -> new IllegalArgumentException("Unsupported flag: " + flagName));
    try {
      final JProlContext prolContext = this.engineContext.get().findOrMakeJProlContext();
      return prolContext.getSystemFlag(flag);
    } catch (Exception e) {
      throw new ScriptException(e);
    }
  }

  /**
   * {@inheritDoc}
   * <p>Only {@link ScriptContext#GLOBAL_SCOPE} and {@link ScriptContext#ENGINE_SCOPE} are supported.
   */
  @Override
  public Bindings getBindings(final int scope) {
    this.assertNotClosed();
    switch (scope) {
      case ScriptContext.ENGINE_SCOPE:
        return this.engineContext.get().getBindings(ScriptContext.ENGINE_SCOPE);
      case ScriptContext.GLOBAL_SCOPE:
        return this.engineContext.get().getBindings(ScriptContext.GLOBAL_SCOPE);
      default:
        throw new IllegalArgumentException("Invalid scope value.");
    }
  }

  private Object commonProveQueryOnce(final Bindings bindings,
                                      final JProlChoicePoint goal) {
    this.assertNotClosed();
    final Term result = goal.prove();
    if (result != null) {
      final Map<String, Object> groundedVars = extractGroundedVariables(goal);
      if (bindings != null) {
        bindings.putAll(groundedVars);
      }
      return Boolean.TRUE;
    }
    return Boolean.FALSE;
  }

  Object proveQueryOnce(final String queryString, final ScriptContext context,
                        final Bindings bindings) {
    assertNotClosed();
    final JProlContext prolContext = asJProlContext(context).findOrMakeJProlContext();
    return this.commonProveQueryOnce(bindings, prolContext.makeChoicePoint(queryString));
  }

  Object proveQueryOnce(final Term queryTerm, final ScriptContext context,
                        final Bindings bindings) {
    assertNotClosed();
    final JProlContext prolContext = asJProlContext(context).findOrMakeJProlContext();
    return this.proveQueryOnce(queryTerm, prolContext, bindings);
  }

  Object proveQueryOnce(final Term queryTerm, final JProlContext prolContext,
                        final Bindings bindings) {
    assertNotClosed();
    return this.commonProveQueryOnce(bindings, prolContext.makeChoicePoint(queryTerm));
  }

  /**
   * {@link Invocable} bridge: proves {@code name(Args...)} once on this engine's current thread context.
   * <p>Unlike generic JSR 223 engines, {@code invokeMethod} is only supported when {@code thisObject} is {@code null},
   * {@code this}, or a {@link JProlScriptEngineProvider} whose {@linkplain JProlScriptEngineProvider#getJProlScriptEngine() engine}
   * is this instance; otherwise {@link ScriptException} is thrown.
   *
   * @return {@link Boolean#TRUE} if the goal succeeds, {@link Boolean#FALSE} otherwise
   */
  @Override
  public Object invokeMethod(final Object thisObject, final String name, final Object... args)
      throws ScriptException {
    this.assertNotClosed();
    if (thisObject != null && thisObject != this) {
      if (!(thisObject instanceof JProlScriptEngineProvider)
          || ((JProlScriptEngineProvider) thisObject).getJProlScriptEngine() != this) {
        throw new ScriptException(
            "invokeMethod is only supported when thisObject is this engine or a JProlScriptEngineProvider for it");
      }
    }
    return this.invokeFunction(name, args);
  }

  /**
   * Proves {@code name(Args...)} once against the current thread's {@link JProlContext}; results are written to
   * {@link ScriptContext#ENGINE_SCOPE} bindings.
   *
   * @return {@link Boolean#TRUE} if the goal succeeds, {@link Boolean#FALSE} otherwise
   */
  @Override
  public Object invokeFunction(final String name, final Object... args)
      throws ScriptException {
    this.assertNotClosed();

    try {
      final Term[] terms = new Term[args.length];
      for (int i = 0; i < args.length; i++) {
        terms[i] = java2term(args[i]);
      }
      final Term term = Terms.newStruct(name, terms, SourcePosition.UNKNOWN);
      final JProlScriptEngineContext scriptEngineContext = this.engineContext.get();
      return this.proveQueryOnce(term, scriptEngineContext,
          scriptEngineContext.getBindings(ScriptContext.ENGINE_SCOPE));
    } catch (Exception ex) {
      throw new ScriptException(ex);
    }
  }

  /**
   * Resolves a view of the engine for the given interface type when {@code thisObject} is a {@link JProlScriptEngineProvider}.
   * Supported types include {@link JProlScriptEngine}, {@link JProlScriptEngineContext}, {@link ScriptContext},
   * {@link JProlContext}, {@link ParserContext}, and {@link KnowledgeBase}; otherwise returns {@code null}.
   *
   * @param targetClass requested type
   * @param <T>         requested type parameter
   * @return adapter instance or {@code null}
   */
  @Override
  public <T> T getInterface(final Class<T> targetClass) {
    return this.getInterface(this, targetClass);
  }

  /**
   * Same as {@link #getInterface(Class)} but anchored to {@code thisObject} (must be a {@link JProlScriptEngineProvider}).
   *
   * @param thisObject  provider whose engine is queried
   * @param targetClass requested type
   * @param <T>         requested type parameter
   * @return adapter instance or {@code null}
   * @throws NullPointerException     if {@code targetClass} is null
   * @throws IllegalArgumentException if {@code thisObject} is not a {@link JProlScriptEngineProvider}
   */
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
        final JProlScriptEngineContext scriptEngineContext = this.engineContext.get();
        if (scriptEngineContext != null
            && (targetClass.isAssignableFrom(ScriptContext.class) ||
            targetClass.isAssignableFrom(JProlScriptEngineContext.class))) {
          return (T) scriptEngineContext;
        }

        final JProlContext prolContext = scriptEngineContext == null ? null
            : scriptEngineContext.findJProlContext();
        if (prolContext != null && targetClass.isAssignableFrom(JProlContext.class)) {
          result = (T) prolContext;
        } else if (prolContext != null && targetClass.isAssignableFrom(ParserContext.class)) {
          result = (T) prolContext.makeParserContext();
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

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isDisposed() {
    return this.disposed.get();
  }

  /**
   * {@inheritDoc}
   * <p>Disposes this engine and, by default, its {@link JProlScriptEngineContext}.
   */
  @Override
  public void dispose() {
    this.dispose(true);
  }

  /**
   * Dispose the object, remove internal state.
   *
   * @param disposeContext if true then engine context also will be disposed
   */
  public void dispose(final boolean disposeContext) {
    if (this.disposed.compareAndSet(false, true)) {
      final JProlScriptEngineContext context = this.engineContext.getAndSet(null);
      if (context != null) {
        if (disposeContext) {
          context.dispose();
        } else {
          context.gc();
        }
      }
    }
  }

  /**
   * {@inheritDoc}
   * <p>Equivalent to {@link #dispose()}.
   */
  @Override
  public void close() {
    this.dispose();
  }

  /**
   * Set prolog system flag state.
   *
   * @param name  name of flag
   * @param value new value of flag
   * @see JProlSystemFlag
   */
  public void setFlag(final String name, final Object value) {
    this.assertNotClosed();
    final JProlSystemFlag flag = JProlSystemFlag.find(newAtom(name))
        .orElseThrow(() -> new IllegalArgumentException("Unsupported flag: " + name));
    final JProlContext prolContext = this.engineContext.get().findOrMakeJProlContext();
    prolContext.setSystemFlag(flag, java2term(value));
  }

  /**
   * Approximate number of items presented in internal script engine state.
   *
   * @return 0 if no state and size if presented
   * @see JProlScriptEngineContext#size()
   */
  public int size() {
    this.assertNotClosed();
    final JProlScriptEngineContext context = this.engineContext.get();
    return context == null ? 0 : context.size();
  }

  /**
   * {@inheritDoc}
   * <p>Runs {@linkplain JProlScriptEngineContext#gc() garbage collection} on the underlying context (stale thread locals).
   */
  @Override
  public void gc() {
    if (!this.disposed.get()) {
      final JProlScriptEngineContext context = this.engineContext.get();
      if (context != null) {
        context.gc();
      }
    }
  }

  @Deprecated
  protected void finalize() {
    this.dispose();
  }
}