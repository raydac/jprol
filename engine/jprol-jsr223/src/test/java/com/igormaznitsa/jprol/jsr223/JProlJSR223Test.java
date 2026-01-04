package com.igormaznitsa.jprol.jsr223;

import static com.igormaznitsa.jprol.jsr223.NamedList.namedListOf;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.igormaznitsa.jprol.data.TermVar;
import com.igormaznitsa.jprol.data.Terms;
import com.igormaznitsa.jprol.libs.JProlCoreLibrary;
import com.igormaznitsa.jprol.libs.JProlIoLibrary;
import com.igormaznitsa.jprol.logic.JProlContext;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import javax.script.Bindings;
import javax.script.Compilable;
import javax.script.CompiledScript;
import javax.script.Invocable;
import javax.script.ScriptContext;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineFactory;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;
import javax.script.SimpleScriptContext;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class JProlJSR223Test {

  private ScriptEngineManager manager;

  @BeforeEach
  void setUp() {
    this.manager = new ScriptEngineManager();
  }

  // ==================== Engine Discovery Tests ====================

  @Test
  @Order(1)
  @DisplayName("Test ScriptEngineManager is not null")
  void testScriptEngineManagerCreation() {
    assertNotNull(manager, "ScriptEngineManager should be created successfully");
  }

  @Test
  @Order(2)
  @DisplayName("Test available script engines discovery")
  void testAvailableEngines() {
    List<ScriptEngineFactory> factories = manager.getEngineFactories();
    assertNotNull(factories, "Engine factories list should not be null");

    System.out.println("\n=== Available Script Engines ===");
    for (final ScriptEngineFactory factory : factories) {
      System.out.printf("Engine: %s %s (%s)%n",
          factory.getEngineName(),
          factory.getEngineVersion(),
          factory.getLanguageName());
      System.out.printf("  Names: %s%n", factory.getNames());
      System.out.printf("  MIME types: %s%n", factory.getMimeTypes());
      System.out.printf("  Extensions: %s%n", factory.getExtensions());
    }
  }

  @ParameterizedTest
  @ValueSource(strings = {"jprol", "jprol.prolog"})
  @DisplayName("Test engine lookup by different names")
  void testEngineByName(String name) {
    ScriptEngine engine = this.manager.getEngineByName(name);
    assertNotNull(engine, name);
    System.out.printf("Found engine for name '%s': %s%n", name,
        engine.getFactory().getEngineName());
    assertNotNull(engine.getFactory());
  }

  @ParameterizedTest
  @ValueSource(strings = {"application/x-prolog", "application/jprol", "text/x-prolog"})
  @DisplayName("Test engine lookup by MIME type")
  void testEngineByMimeType(String mimeType) {
    ScriptEngine engine = manager.getEngineByMimeType(mimeType);
    assertNotNull(engine, mimeType);
      System.out.printf("Found engine for MIME type '%s': %s%n", mimeType,
          engine.getFactory().getEngineName());
  }

  @ParameterizedTest
  @ValueSource(strings = {"pl", "pro", "prolog"})
  @DisplayName("Test engine lookup by extension")
  void testEngineByExtension(String extension) {
    ScriptEngine engine = manager.getEngineByExtension(extension);
    assertNotNull(engine, extension);
      System.out.printf("Found engine for extension '%s': %s%n", extension,
          engine.getFactory().getEngineName());
  }

  // ==================== Basic Evaluation Tests ====================

  @Test
  @DisplayName("Test simple arithmetic evaluation")
  void testSimpleArithmetic() throws ScriptException {
    ScriptEngine engine = getAvailableEngine();
    assertNotNull(engine);

    assertTrue((boolean) engine.eval("?- Z is 2 + 2."));
    assertEquals(4L, engine.get("Z"));
  }

  @Test
  @DisplayName("Test string concatenation")
  void testStringConcatenation() throws ScriptException {
    ScriptEngine engine = getAvailableEngine();
    assertNotNull(engine);
    assertTrue((boolean) engine.eval("?-atom_concat('Hello',' World',X)."));
    assertEquals("Hello World", engine.get("X"));
  }

  @Test
  @DisplayName("Test variable declaration and usage")
  void testVariableUsage() throws ScriptException {
    ScriptEngine engine = getAvailableEngine();
    assertNotNull(engine);

    assertTrue((boolean) engine.eval("valueX(10). valueY(20)."));
    assertTrue((boolean) engine.eval("?- valueX(X), valueY(Y), Z is X + Y."));
    assertEquals(30L, engine.get("Z"));
  }

  @Test
  @DisplayName("Test function definition and invocation")
  void testFunctionDefinition() throws ScriptException {
    ScriptEngine engine = getAvailableEngine();
    assertNotNull(engine);

    String script = "add(A,B,C) :- C is A + B. ?- add(5,7,C).";

    assertTrue((boolean) engine.eval(script));
    assertEquals(12L, engine.get("C"));
  }

  // ==================== Bindings Tests ====================

  @Test
  @DisplayName("Test engine scope bindings")
  void testEngineBindings() throws ScriptException {
    ScriptEngine engine = getAvailableEngine();
    if (engine == null) {
      return;
    }

    Bindings bindings = engine.createBindings();
    bindings.put("X", 42);
    bindings.put("Name", "Java");

    assertTrue((boolean) engine.eval("?-Y is X.", bindings));
    assertEquals(42L, bindings.get("Y"));
  }

  @Test
  @DisplayName("Test global scope bindings")
  void testGlobalBindings() throws ScriptException {
    ScriptEngine engine = getAvailableEngine();
    assertNotNull(engine);
    manager.put("GlobalVar", 100);
    engine.getBindings(ScriptContext.ENGINE_SCOPE).put("LocalVar", 50);

    assertTrue((boolean) engine.eval("?- Y is LocalVar, X is GlobalVar."));
    assertEquals(50L, engine.get("Y"));
    assertEquals(100L, engine.get("X"));
  }

  @Test
  @DisplayName("Test bindings scope hierarchy")
  void testBindingsScopeHierarchy() throws ScriptException {
    ScriptEngine engine = getAvailableEngine();
    assertNotNull(engine);

    engine.put("Test", "engine_scope");
    Bindings bindings = engine.createBindings();
    bindings.put("Test", "custom_scope");

    assertTrue((boolean) engine.eval("?- Y = Test.", bindings));
    assertEquals("custom_scope", bindings.get("Y"));
    assertNull(engine.get("Y"));
  }

  @Test
  @DisplayName("Test object binding")
  void testObjectBinding() throws ScriptException {
    ScriptEngine engine = getAvailableEngine();
    assertNotNull(engine);

    List<String> list = List.of("A", "B", "C");
    engine.put("X", list);

    assertTrue((boolean) engine.eval("?- length(X,Y)."));
    assertEquals(3L, engine.get("Y"));
  }

  // ==================== Context Tests ====================

  @Test
  @DisplayName("Test ScriptContext manipulation")
  void testScriptContext() {
    ScriptEngine engine = getAvailableEngine();
    assertNotNull(engine);

    ScriptContext context = new SimpleScriptContext();
    context.setAttribute("contextVar", 999, ScriptContext.ENGINE_SCOPE);

    Bindings bindings = context.getBindings(ScriptContext.ENGINE_SCOPE);
    assertNotNull(bindings);
    assertEquals(999, bindings.get("contextVar"));
  }

  @Test
  @DisplayName("Test custom reader and writer")
  void testCustomReaderWriter() throws ScriptException {
    ScriptEngine engine = getAvailableEngine();
    assertNotNull(engine);

    StringWriter writer = new StringWriter();
    StringWriter errorWriter = new StringWriter();

    ScriptContext context = engine.getContext();
    context.setWriter(writer);
    context.setErrorWriter(errorWriter);

    assertTrue((boolean) engine.eval("?- write('Hello from script').", context));
    assertEquals("", writer.toString());
  }

  // ==================== Invocable Interface Tests ====================

  @Test
  @DisplayName("Test Invocable interface - function invocation")
  void testInvocableFunction() throws ScriptException, NoSuchMethodException {
    ScriptEngine engine = getAvailableEngine();
    assertInstanceOf(Invocable.class, engine);

    String script = "greet(Name, Result) :- atom_concat('Hello ', Name, Result).";
    assertTrue((boolean) engine.eval(script));

    Invocable invocable = (Invocable) engine;
    assertTrue((boolean) invocable.invokeFunction("greet", "World", Terms.newVar("Y")));
    assertEquals("Hello World", engine.get("Y"));
  }

  @Test
  @DisplayName("Test Invocable interface - method invocation")
  void testInvocableMethod() throws ScriptException, NoSuchMethodException {
    ScriptEngine engine = getAvailableEngine();
    assertNotNull(engine);

    String script = "multiply(A,B,C):-C is A * B.";
    assertTrue((boolean) engine.eval(script));
    Invocable invocable = (Invocable) engine;

    final TermVar result = Terms.newVar("R");
    assertTrue((boolean) invocable.invokeMethod(null, "multiply", 6, 7, result));
    assertEquals(42L, result.getValue().toNumber().intValue());
  }

  @Test
  @DisplayName("Test interface implementation")
  void testInterfaceImplementation() throws ScriptException {
    ScriptEngine engine = getAvailableEngine();
    assertInstanceOf(Invocable.class, engine);

    String script = "result(X) :- X = 'Running...'.";
    assertNotNull(engine.eval(script));

    Invocable invocable = (Invocable) engine;
    JProlContext context = invocable.getInterface(JProlContext.class);
    assertNotNull(context);
    assertEquals("Running...",
        context.makeChoicePoint("result(X).").prove().findAllNamedVariables().get("X").getValue()
            .getText());
  }

  // ==================== Compilable Interface Tests ====================

  @Test
  @DisplayName("Test script compilation")
  void testScriptCompilation() throws ScriptException {
    ScriptEngine engine = getAvailableEngine();
    assertInstanceOf(Compilable.class, engine);

    Compilable compilable = (Compilable) engine;
    CompiledScript compiled = compilable.compile("?- X is 5 * 5.");

    assertNotNull(compiled);
    assertNotNull(compiled.eval());
    assertEquals(25L, compiled.getEngine().get("X"));
  }

  @Test
  @DisplayName("Test compiled script reusability")
  void testCompiledScriptReuse() throws ScriptException {
    ScriptEngine engine = getAvailableEngine();
    assertInstanceOf(Compilable.class, engine);

    Compilable compilable = (Compilable) engine;
    CompiledScript compiled = compilable.compile("?- Y is X * 2.");

    Bindings bindings1 = engine.createBindings();
    bindings1.put("X", 10);

    Bindings bindings2 = engine.createBindings();
    bindings2.put("X", 20);

    assertTrue((boolean) compiled.eval(bindings1));
    assertTrue((boolean) compiled.eval(bindings2));

    assertEquals(20L, bindings1.get("Y"));
    assertEquals(40L, bindings2.get("Y"));
  }

  // ==================== Error Handling Tests ====================

  @Test
  @DisplayName("Test syntax error handling")
  void testSyntaxError() {
    ScriptEngine engine = getAvailableEngine();
    assertNotNull(engine);

    assertThrows(ScriptException.class, () -> {
      engine.eval("this is not valid syntax @#$");
    });
  }

  @Test
  @DisplayName("Test runtime error handling")
  void testRuntimeError() {
    ScriptEngine engine = getAvailableEngine();
    assertNotNull(engine);

    assertThrows(ScriptException.class, () -> {
      engine.eval("throw new Error('Test error');");
    });
  }

  @Test
  @DisplayName("Test non-instantiated variable")
  void testNonInstantiatedVariable() throws ScriptException {
    ScriptEngine engine = getAvailableEngine();
    assertNotNull(engine);
    assertThrows(ScriptException.class, () -> engine.eval("?-X."));
  }

  // ==================== Advanced Features Tests ====================

  @Test
  @DisplayName("Test object creation and property access")
  void testObjectCreation() throws ScriptException {
    ScriptEngine engine = getAvailableEngine();
    assertNotNull(engine);

    String script = "?- Person = [name/'John', age/30].";

    assertTrue((boolean) engine.eval(script));
    final Object objectResult = engine.get("Person");
    assertEquals(List.of(namedListOf("/", "name", "John"), namedListOf("/", "age", 30L)),
        objectResult);
  }

  @Test
  @DisplayName("Test loop execution")
  void testLoopExecution() throws ScriptException {
    ScriptEngine engine = getAvailableEngine();
    assertNotNull(engine);

    String script =
        "sum_1_to_10(Sum) :-"
            + "    sum_range(1, 10, 0, Sum)."
            + "sum_range(I, Max, Acc, Sum) :-"
            + "    I =< Max,"
            + "    Acc1 is Acc + I,"
            + "    I1 is I + 1,"
            + "    sum_range(I1, Max, Acc1, Sum)."
            + "sum_range(I, Max, Sum, Sum) :-"
            + "    I > Max."
            + "?- sum_1_to_10(S).";

    assertTrue((boolean) engine.eval(script));
    assertEquals(55L, engine.get("S"));
  }

  @Test
  @DisplayName("Test conditional execution")
  void testConditionalExecution() throws ScriptException {
    ScriptEngine engine = getAvailableEngine();
    assertNotNull(engine);

    engine.put("X", Terms.newLong(15));
    String script = "test(X,Y):- X > 10 -> Y = 'high' ; Y = 'low'. ?- test(X,Y).";

    assertTrue((boolean) engine.eval(script));
    assertEquals("high", engine.get("Y"));
  }

  // ==================== Multi-threading Tests ====================

  @Test
  @DisplayName("Test concurrent script execution")
  void testConcurrentExecution() throws InterruptedException, ExecutionException {
    ScriptEngine engine = getAvailableEngine();
    assertNotNull(engine);

    final ExecutorService executor = Executors.newFixedThreadPool(3);
    try {
      List<Future<Bindings>> futures = new ArrayList<>();

      for (int i = 0; i < 55; i++) {
        final int value = i;
        futures.add(executor.submit(() -> {
          try {
            Bindings bindings = engine.createBindings();
            bindings.put("N", value);
            Object result = engine.eval("?- Y is N * N.", bindings);
            return (boolean) result ? bindings : null;
          } catch (ScriptException e) {
            throw new RuntimeException(e);
          }
        }));
      }

      for (int i = 0; i < futures.size(); i++) {
        final Bindings bindings = futures.get(i).get();
        assertEquals((long) i * i, bindings.get("Y"));
      }
    } finally {
      executor.shutdown();
      assertTrue(executor.awaitTermination(5, TimeUnit.SECONDS));

    }
  }

  // ==================== Type Conversion Tests ====================

  @Test
  @DisplayName("Test Java to Script type conversion")
  void testJavaToScriptConversion() throws ScriptException {
    ScriptEngine engine = getAvailableEngine();
    assertNotNull(engine);

    engine.put("intVal", 42);
    engine.put("doubleVal", 3.14);
    engine.put("boolVal", true);
    engine.put("stringVal", "test");

    assertNotNull(engine.get("intVal"));
    assertNotNull(engine.get("doubleVal"));
    assertNotNull(engine.get("boolVal"));
    assertNotNull(engine.get("stringVal"));
  }

  @Test
  @DisplayName("Test Script to Java type conversion")
  void testScriptToJavaConversion() throws ScriptException {
    ScriptEngine engine = getAvailableEngine();
    assertNotNull(engine);

    assertTrue((boolean) engine.eval("?- N=100, T='text', L=[1,2,3]."));

    assertEquals(100L, engine.getBindings(ScriptContext.ENGINE_SCOPE).get("N"));
    assertEquals("text", engine.getBindings(ScriptContext.ENGINE_SCOPE).get("T"));
    assertEquals(List.of(1L, 2L, 3L), engine.getBindings(ScriptContext.ENGINE_SCOPE).get("L"));
  }

  // ==================== Helper Methods ====================

  private ScriptEngine getAvailableEngine() {
    this.manager.put(JProlScriptEngine.JPROL_LIBRARIES,
        List.of(new JProlCoreLibrary(), new JProlIoLibrary()));
    return this.manager.getEngineByName("jprol");
  }

  @AfterEach
  void tearDown() {
    this.manager = null;
  }
}