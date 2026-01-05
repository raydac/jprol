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
import com.igormaznitsa.prologparser.exceptions.PrologParserException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.StringReader;
import java.io.Writer;
import java.nio.charset.Charset;
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
 * JProl JSR 223 script engine implementation.
 * Allows execution of Prolog code through the standard Java Scripting API.
 * <b>It has MULTITHREADED level, threads not isolated</b>
 * <p>
 * Supports custom libraries via ScriptContext attributes:
 * - "jprol.libraries" - list of {@link com.igormaznitsa.jprol.libs.AbstractJProlLibrary} instances to be loaded during new {@link JProlContext} create or reinit
 * - "jprol.context.flags" - {@link java.util.Map} of {@code String} to {@code Object} describing JProl engine flags
 * <p>
 * It is possible define in global context listed attributes:
 * - "jprol.global.executor.service" - {@link java.util.concurrent.ExecutorService} service for async tasks
 * - "jprol.global.knowledge.base" - shared {@link KnowledgeBase} between all child engines
 * - "jprol.global.critical.predicate.guard" - can be provided as a {@link JProlGuardPredicate} to disable execution of predicates marked in library as critical ones
 *
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
        result = new InputStreamReader(System.in, Charset.defaultCharset());
      }
      return result;
    }

    @Override
    public Writer findWriter(final JProlContext context, final String writerId,
                             final boolean append) {
      if (WRITER_USER.equals(writerId)) {
        return new PrintWriter(System.out, true, Charset.defaultCharset());
      } else if (WRITER_ERR.equals(writerId)) {
        return new PrintWriter(System.err, true, Charset.defaultCharset());
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

  @Override
  public JProlScriptEngine getJProlScriptEngine() {
    return this;
  }

  @Override
  public Object eval(String script) throws ScriptException {
    return this.eval(script, this.engineContext.get());
  }

  @Override
  public Object eval(final Reader reader) throws ScriptException {
    return this.eval(reader, this.engineContext.get());
  }

  @Override
  public Object eval(final String script, final Bindings bindings) throws ScriptException {
    return this.eval(new StringReader(script), this.engineContext.get(), bindings);
  }

  @Override
  public Object eval(final Reader reader, final Bindings bindings) throws ScriptException {
    this.assertNotClosed();
    return this.eval(reader, this.engineContext.get(), bindings);
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
  public Object get(final String key) {
    this.assertNotClosed();
    final Bindings bindings = this.getBindings(ScriptContext.ENGINE_SCOPE);
    if (bindings != null) {
      return bindings.get(key);
    }
    return null;
  }

  @Override
  public ScriptContext getContext() {
    this.assertNotClosed();
    return this.engineContext.get();
  }

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
   * Find '?-' query goal in script and prove it once, place results into engine scoped bindings.
   *
   * @param script  prolog script
   * @param context target context
   * @return {@link Boolean#TRUE} if proved, {@link Boolean#FALSE} otherwise
   * @throws ScriptException if any internal error
   */
  @Override
  public Object eval(final String script, final ScriptContext context) throws ScriptException {
    return this.eval(new StringReader(script), context, null);
  }

  /**
   * Find '?-' query goal in script and prove it once, place results into target bindings if presented or into engine scoped bindings.
   *
   * @param script               prolog script
   * @param context              target context
   * @param customEngineBindings target bindings
   * @return {@link Boolean#TRUE} if proved, {@link Boolean#FALSE} otherwise
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
      final JProlContext prolContext = this.engineContext.get().findOrMakeJProlContext();

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

  @Override
  public Object eval(final Reader reader, final ScriptContext context) throws ScriptException {
    return this.eval(reader, context, null);
  }

  @Override
  public Bindings createBindings() {
    this.assertNotClosed();
    return new SimpleBindings();
  }

  @Override
  public void setBindings(final Bindings bindings, final int scope) {
    this.assertNotClosed();
    this.engineContext.get().setBindings(bindings, scope);
  }

  @Override
  public ScriptEngineFactory getFactory() {
    this.assertNotClosed();
    return this.factory;
  }

  @Override
  public CompiledScript compile(final String script) throws ScriptException {
    this.assertNotClosed();
    return new JProlCompiledScript(this, script);
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

  /**
   * Prove query and return all resulted variables as list.
   *
   * @param queryString query, must not contain '?-'
   * @return list of variables for proved variants, can't be null
   * @throws ScriptException if any error during execution
   */
  public List<Map<String, Object>> query(final String queryString) throws ScriptException {
    this.assertNotClosed();
    final List<Map<String, Object>> results = new ArrayList<>();
    final JProlContext prolContext = this.engineContext.get().findOrMakeJProlContext();
    try {
      final JProlChoicePoint goal = prolContext.makeChoicePoint(queryString);
      while (goal.prove() != null) {
        results.add(extractGroundedVariables(goal));
      }
    } catch (PrologParserException e) {
      if (e.hasValidPosition()) {
        throw new ScriptException(
            "Error parsing query: " + e.getMessage() + " " + e.getLine() + ':' + e.getPos());
      } else {
        throw new ScriptException(e);
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

  @Override
  public Object invokeMethod(final Object thisObject, final String name, final Object... args)
      throws ScriptException {
    return this.invokeFunction(name, args);
  }

  /**
   * It will prove once a goal to simulate function call
   *
   * @param name of target predicate
   * @param args arguments of predicate to prove
   * @return {@link Boolean#TRUE} if proven, {@link Boolean#FALSE} otherwise
   * @throws ScriptException if any internal error
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
   * Find internal object with specified interface.
   *
   * @param targetClass The <code>Class</code> object of the interface to return.
   * @param <T>         type of needed object
   * @return found object or null
   */
  @Override
  public <T> T getInterface(final Class<T> targetClass) {
    return this.getInterface(this, targetClass);
  }

  /**
   * Find internal object with specified interface.
   *
   * @param thisObject  target object to find internal object implementing target class
   * @param targetClass The <code>Class</code> object of the interface to return.
   * @param <T>         type of needed object
   * @return found object or null
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
  public boolean isDisposed() {
    return this.disposed.get();
  }

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

  @Override
  public void gc() {
    this.assertNotClosed();
    final JProlScriptEngineContext context = this.engineContext.get();
    if (context != null) {
      context.gc();
    }
  }
}