package com.igormaznitsa.jprol.jsr223;

import static com.igormaznitsa.jprol.jsr223.JProlJsr223BootstrapLibrary.READER_IN;
import static com.igormaznitsa.jprol.jsr223.JProlJsr223BootstrapLibrary.WRITER_ERR;
import static com.igormaznitsa.jprol.jsr223.JProlJsr223BootstrapLibrary.WRITER_OUT;
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
import java.util.HashMap;
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
 * <b>It has MULTIHREADED level, threads not isolated</b>
 * <p>
 * Supports custom libraries via ScriptContext attributes:
 * - "jprol.libraries" - list of {@link com.igormaznitsa.jprol.libs.AbstractJProlLibrary} instances to be loaded during new {@link JProlContext} create or reinit
 * - "jprol.context.flags" - {@link java.util.Map} of {@code String} to {@code Object} describing JProl engine flags
 * <p>
 * It is possible define in global context listed attributes:
 * - "jprol.global.executor.service" - {@link java.util.concurrent.ExecutorService} service for async tasks
 * - "jprol.global.knowledge.base" - shared {@link KnowledgeBase} between all child engines
 * - "jprol.global.critical.predicate.guard" - can be provided as a {@link JProlCriticalPredicateGuard} to disable execution of predicates marked in library as critical ones
 *
 * @since 2.2.2
 */
public class JProlScriptEngine
    implements ScriptEngine, Compilable, Invocable, JProlScriptEngineProvider, AutoCloseable {

  /**
   * Checker to allow execution of critical predicates like knowledge base operations.
   *
   * @see JProlCriticalPredicateGuard
   */
  public static final String JPROL_GLOBAL_CRITICAL_PREDICATE_GUARD =
      "jprol.global.critical.predicate.guard";

  /**
   * Executor service to be used for all child JProl engines to start async processes. Can be defined only in global scope.
   */
  public static final String JPROL_GLOBAL_EXECUTOR_SERVICE = "jprol.global.executor.service";

  /**
   * Knowledge base to be used for all child JProl engines. Can be defined only in global scope.
   */
  public static final String JPROL_GLOBAL_KNOWLEDGE_BASE = "jprol.global.knowledge.base";

  /**
   * Array of JProl libraries which will be applied to JProl engine context during create. Can be defined in both global and engine context.
   */
  public static final String JPROL_LIBRARIES = "jprol.libraries";

  /**
   * Map of JProl flag names as String and their values as objects to be applied during JProl engine initialization. Can be defined in both global and engine context.
   */
  public static final String JPROL_CONTEXT_FLAGS = "jprol.context.flags";

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
  private final JProlScriptEngineFactory factory;
  private final AtomicBoolean closed = new AtomicBoolean();
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

  /**
   * Disposing existing thread local JProl context and create new one from current context parameters.
   * Knowledge base can be affected.
   */
  public void reinitJProlContext() {
    this.assertNotClosed();
    this.engineContext.get().reinitJProlContext();
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
    this.engineContext.set(asJProlContext(requireNonNull(context)));
  }

  @Override
  public Object eval(final String script, final ScriptContext context) throws ScriptException {
    return this.eval(new StringReader(script), context, null);
  }

  public Object eval(final Reader script, final ScriptContext context,
                     final Bindings customEngineBindings) throws ScriptException {
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
      return this.executeQuery(queryString, context,
          customEngineBindings == null ? context.getBindings(ScriptContext.ENGINE_SCOPE) :
              customEngineBindings);
    } catch (Exception e) {
      if (e instanceof PrologParserException) {
        final PrologParserException pe = (PrologParserException) e;
        if (pe.hasValidPosition()) {
          throw new ScriptException(
              "Error parsing Prolog script: " + e);
        } else {
          throw new ScriptException(
              "Error parsing Prolog script: " + e);
        }
      }
      throw new ScriptException("Error executing Prolog script: " + e);
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
      return this.eval(sb.toString(), context);
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

  public List<Map<String, Object>> query(final String queryString) throws ScriptException {
    this.assertNotClosed();
    final List<Map<String, Object>> results = new ArrayList<>();
    final JProlContext prolContext = this.engineContext.get().findOrMakeJProlContext();
    try {
      final JProlChoicePoint goal = new JProlChoicePoint(queryString, prolContext);
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
      this.engineContext.get().findOrMakeJProlContext().consult(new StringReader(source));
    } catch (Exception e) {
      throw new ScriptException("Error consulting Prolog source: " + e.getMessage());
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

  Object executeQuery(final String queryString, final ScriptContext context,
                      final Bindings bindings) {
    this.assertNotClosed();
    final JProlContext prolContext = asJProlContext(context).findOrMakeJProlContext();
    final JProlChoicePoint goal = new JProlChoicePoint(queryString, prolContext);
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

  Object executeQuery(final Term queryTerm, final ScriptContext context, final Bindings bindings) {
    this.assertNotClosed();
    final JProlContext prolContext = asJProlContext(context).findOrMakeJProlContext();
    final JProlChoicePoint goal = new JProlChoicePoint(queryTerm, prolContext);
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

  Object executeQuery(final Term queryTerm, final JProlContext prolContext,
                      final Bindings bindings) {
    this.assertNotClosed();
    final JProlChoicePoint goal = new JProlChoicePoint(queryTerm, prolContext);
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

  @Override
  public Object invokeMethod(final Object thisObject, final String name, final Object... args)
      throws ScriptException {
    return this.invokeFunction(name, args);
  }

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
      return this.executeQuery(term, scriptEngineContext,
          scriptEngineContext.getBindings(ScriptContext.ENGINE_SCOPE));
    } catch (Exception ex) {
      throw new ScriptException(ex);
    }
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
        final JProlContext prolContext = this.engineContext.get().findJProlContext();
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

  public void dispose(final boolean disposeContext) {
    if (this.closed.compareAndSet(false, true)) {
      final JProlScriptEngineContext context = this.engineContext.getAndSet(null);
      if (context != null && disposeContext) {
        context.dispose();
      }
    }
  }

  @Override
  public void close() {
    this.dispose(true);
  }

  public void setFlag(final String name, final Object value) {
    this.assertNotClosed();
    final String text = String.format(":- set_prolog_flag('%s', %s).", ProlUtils.escapeSrc(name),
        java2term(value).toSrcString());
    final JProlContext prolContext = this.engineContext.get().findOrMakeJProlContext();
    prolContext.consult(new StringReader(text));
  }

  public int size() {
    this.assertNotClosed();
    final JProlScriptEngineContext context = this.engineContext.get();
    return context == null ? 0 : context.size();
  }
}