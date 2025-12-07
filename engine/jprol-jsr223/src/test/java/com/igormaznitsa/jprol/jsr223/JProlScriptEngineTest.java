package com.igormaznitsa.jprol.jsr223;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.igormaznitsa.jprol.annotations.JProlPredicate;
import com.igormaznitsa.jprol.data.NumericTerm;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermList;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.data.Terms;
import com.igormaznitsa.jprol.libs.AbstractJProlLibrary;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import javax.script.Bindings;
import javax.script.ScriptContext;
import javax.script.ScriptException;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.TestMethodOrder;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class JProlScriptEngineTest {

  private JProlScriptEngineFactory factory;

  @BeforeAll
  public void setUp() {
    factory = new JProlScriptEngineFactory();
    System.out.println("=== JProl Custom Library Tests ===\n");
  }

  @Test
  @Order(1)
  @DisplayName("Test 1: Basic Custom Library with Simple Predicates")
  public void testBasicCustomLibrary() throws ScriptException {
    System.out.println("Test 1: Basic Custom Library");
    System.out.println("-----------------------------");

    JProlScriptEngine engine = (JProlScriptEngine) factory.getScriptEngine(
        new BasicCustomLibrary()
    );

    // Test hello_world/0 predicate
    engine.consult("test :- hello_world.");
    List<Map<String, Object>> results = engine.query("test.");

    assertNotNull(results, "Results should not be null");
    assertFalse(results.isEmpty(), "Should have at least one solution");

    System.out.println("✓ hello_world/0 predicate works correctly");

    // Test greet/1 predicate
    results = engine.query("greet('Alice').");
    assertFalse(results.isEmpty(), "greet/1 should succeed");

    System.out.println("✓ greet/1 predicate works correctly");
    System.out.println();
  }

  @Test
  @Order(2)
  @DisplayName("Test 2: Math Library with Arithmetic Operations")
  public void testMathLibrary() throws ScriptException {
    System.out.println("Test 2: Math Library");
    System.out.println("--------------------");

    JProlScriptEngine engine = (JProlScriptEngine) factory.getScriptEngine(
        new MathLibrary()
    );

    // Test square/2
    List<Map<String, Object>> results = engine.query("square(5, X).");
    assertFalse(results.isEmpty(), "square/2 should return result");
    assertEquals(25L, results.get(0).get("X"), "5^2 should be 25");
    System.out.println("✓ square(5, X) => X = " + results.get(0).get("X"));

    // Test cube/2
    results = engine.query("cube(3, X).");
    assertFalse(results.isEmpty(), "cube/2 should return result");
    assertEquals(27L, results.get(0).get("X"), "3^3 should be 27");
    System.out.println("✓ cube(3, X) => X = " + results.get(0).get("X"));

    // Test factorial_custom/2
    results = engine.query("factorial_custom(5, X).");
    assertFalse(results.isEmpty(), "factorial_custom/2 should return result");
    assertEquals(120L, results.get(0).get("X"), "5! should be 120");
    System.out.println("✓ factorial_custom(5, X) => X = " + results.get(0).get("X"));

    // Test is_even/1
    results = engine.query("is_even(4).");
    assertFalse(results.isEmpty(), "is_even(4) should succeed");
    System.out.println("✓ is_even(4) succeeded");

    results = engine.query("is_even(3).");
    assertTrue(results.isEmpty(), "is_even(3) should fail");
    System.out.println("✓ is_even(3) failed as expected");

    // Test is_odd/1
    results = engine.query("is_odd(3).");
    assertFalse(results.isEmpty(), "is_odd(3) should succeed");
    System.out.println("✓ is_odd(3) succeeded");

    System.out.println();
  }

  @Test
  @Order(3)
  @DisplayName("Test 3: String Manipulation Library")
  public void testStringLibrary() throws ScriptException {
    System.out.println("Test 3: String Library");
    System.out.println("----------------------");

    JProlScriptEngine engine = (JProlScriptEngine) factory.getScriptEngine(
        new StringLibrary()
    );

    // Test str_upper/2
    List<Map<String, Object>> results = engine.query("str_upper('hello', X).");
    assertFalse(results.isEmpty(), "str_upper/2 should return result");
    assertEquals("HELLO", results.get(0).get("X"), "Should convert to uppercase");
    System.out.println("✓ str_upper('hello', X) => X = " + results.get(0).get("X"));

    // Test str_lower/2
    results = engine.query("str_lower('WORLD', X).");
    assertFalse(results.isEmpty(), "str_lower/2 should return result");
    assertEquals("world", results.get(0).get("X"), "Should convert to lowercase");
    System.out.println("✓ str_lower('WORLD', X) => X = " + results.get(0).get("X"));

    // Test str_length/2
    results = engine.query("str_length('prolog', X).");
    assertFalse(results.isEmpty(), "str_length/2 should return result");
    assertEquals(6L, results.get(0).get("X"), "'prolog' has 6 characters");
    System.out.println("✓ str_length('prolog', X) => X = " + results.get(0).get("X"));

    // Test str_reverse/2
    results = engine.query("str_reverse('abc', X).");
    assertFalse(results.isEmpty(), "str_reverse/2 should return result");
    assertEquals("cba", results.get(0).get("X"), "Should reverse string");
    System.out.println("✓ str_reverse('abc', X) => X = " + results.get(0).get("X"));

    // Test str_concat_custom/3
    results = engine.query("str_concat_custom('Hello', 'World', X).");
    assertFalse(results.isEmpty(), "str_concat_custom/3 should return result");
    assertEquals("HelloWorld", results.get(0).get("X"), "Should concatenate strings");
    System.out.println(
        "✓ str_concat_custom('Hello', 'World', X) => X = " + results.get(0).get("X"));

    System.out.println();
  }

  @Test
  @Order(4)
  @DisplayName("Test 4: Multiple Libraries Together")
  public void testMultipleLibraries() throws ScriptException {
    System.out.println("Test 4: Multiple Libraries");
    System.out.println("--------------------------");

    JProlScriptEngine engine = (JProlScriptEngine) factory.getScriptEngine(
        new MathLibrary(),
        new StringLibrary()
    );

    // Use predicates from both libraries in one query
    engine.consult(
        "process(N, S, NSq, SU) :- " +
            "  square(N, NSq), " +
            "  str_upper(S, SU)."
    );

    List<Map<String, Object>> results = engine.query("process(5, 'test', X, Y).");
    assertFalse(results.isEmpty(), "Combined query should succeed");

    Map<String, Object> solution = results.get(0);
    assertEquals(25L, solution.get("X"), "Square of 5 should be 25");
    assertEquals("TEST", solution.get("Y"), "Should uppercase 'test'");

    System.out.println("✓ Combined query with multiple libraries works");
    System.out.println("  Square result: " + solution.get("X"));
    System.out.println("  Uppercase result: " + solution.get("Y"));
    System.out.println();
  }

  @Test
  @Order(5)
  @DisplayName("Test 5: Library via ScriptContext")
  public void testLibraryViaContext() throws ScriptException {
    System.out.println("Test 5: Library via Context");
    System.out.println("---------------------------");

    JProlScriptEngine engine = (JProlScriptEngine) this.factory.getScriptEngine();
    ScriptContext context = engine.getContext();

    Object[] libs = new Object[] {new MathLibrary()};
    context.setAttribute("jprol.libraries", libs, ScriptContext.ENGINE_SCOPE);

    final Bindings engineBindings = context.getBindings(ScriptContext.ENGINE_SCOPE);
    engineBindings.put("A", 7);

    engine.eval("?- square(A, X).", context);

    Bindings bindings = context.getBindings(ScriptContext.ENGINE_SCOPE);
    Object result = bindings.get("X");

    assertNotNull(result, "Result should be in bindings");
    assertEquals(49L, result, "Square of 7 should be 49");

    System.out.println("✓ Context-based library loading works");
    System.out.println("  X = " + result);
    System.out.println();
  }

  @Test
  @Order(6)
  @DisplayName("Test 6: Adding Libraries After Creation")
  public void testAddLibrariesAfterCreation() throws ScriptException {
    System.out.println("Test 6: Add Libraries After Creation");
    System.out.println("------------------------------------");

    JProlScriptEngine engine = (JProlScriptEngine) factory.getScriptEngine();

    // Initially, custom predicates should not be available
    assertThrows(ScriptException.class, () -> engine.query("square(5, X)."),
        "square/2 should not be available initially");

    System.out.println("✓ Custom predicate not available before adding library");

    // Add library
    engine.addLibraries(new MathLibrary());

    // Now it should work
    List<Map<String, Object>> results = engine.query("square(5, X).");
    assertFalse(results.isEmpty(), "square/2 should now be available");
    assertEquals(25L, results.get(0).get("X"), "Square of 5 should be 25");

    System.out.println("✓ Custom predicate available after adding library");
    System.out.println("  X = " + results.get(0).get("X"));
    System.out.println();
  }

  @Test
  @Order(7)
  @DisplayName("Test 7: DateTime Library")
  public void testDateTimeLibrary() throws ScriptException {
    System.out.println("Test 7: DateTime Library");
    System.out.println("------------------------");

    JProlScriptEngine engine = (JProlScriptEngine) factory.getScriptEngine(
        new DateTimeLibrary()
    );

    // Test current_timestamp/1
    List<Map<String, Object>> results = engine.query("current_timestamp(X).");
    assertFalse(results.isEmpty(), "current_timestamp/1 should return result");

    Object timestamp = results.get(0).get("X");
    assertNotNull(timestamp, "Timestamp should not be null");
    assertInstanceOf(Long.class, timestamp, "Timestamp should be a Long");
    assertTrue((Long) timestamp > 0, "Timestamp should be positive");

    System.out.println("✓ current_timestamp(X) => X = " + timestamp);

    // Test current_date/3
    results = engine.query("current_date(Y, M, D).");
    assertFalse(results.isEmpty(), "current_date/3 should return result");

    Map<String, Object> solution = results.get(0);
    Object year = solution.get("Y");
    Object month = solution.get("M");
    Object day = solution.get("D");

    assertNotNull(year, "Year should not be null");
    assertNotNull(month, "Month should not be null");
    assertNotNull(day, "Day should not be null");

    assertTrue((Long) year >= 2024, "Year should be reasonable");
    assertTrue((Long) month >= 1 && (Long) month <= 12, "Month should be 1-12");
    assertTrue((Long) day >= 1 && (Long) day <= 31, "Day should be 1-31");

    System.out.println(
        "✓ current_date(Y, M, D) => Y = " + year + ", M = " + month + ", D = " + day);
    System.out.println();
  }

  @Test
  @Order(8)
  @DisplayName("Test 8: Collection Library")
  public void testCollectionLibrary() throws ScriptException {
    System.out.println("Test 8: Collection Library");
    System.out.println("--------------------------");

    JProlScriptEngine engine = (JProlScriptEngine) factory.getScriptEngine(
        new CollectionLibrary()
    );

    // Test list_unique/2
    List<Map<String, Object>> results = engine.query("list_unique([1,2,2,3,3,3], X).");
    assertFalse(results.isEmpty(), "list_unique/2 should return result");

    Object uniqueList = results.get(0).get("X");
    assertNotNull(uniqueList, "Unique list should not be null");
    assertInstanceOf(List.class, uniqueList, "Result should be a list");

    @SuppressWarnings("unchecked")
    List<Object> list = (List<Object>) uniqueList;
    assertEquals(3, list.size(), "Unique list should have 3 elements");

    System.out.println("✓ list_unique([1,2,2,3,3,3], X) => X = " + uniqueList);

    // Test list_sum_custom/2
    results = engine.query("list_sum_custom([1,2,3,4,5], X).");
    assertFalse(results.isEmpty(), "list_sum_custom/2 should return result");
    assertEquals(15L, results.get(0).get("X"), "Sum of [1,2,3,4,5] should be 15");

    System.out.println("✓ list_sum_custom([1,2,3,4,5], X) => X = " + results.get(0).get("X"));
    System.out.println();
  }

  @Test
  @Order(9)
  @DisplayName("Test 9: Complex Integration Test")
  public void testComplexIntegration() throws ScriptException {
    System.out.println("Test 9: Complex Integration");
    System.out.println("---------------------------");

    JProlScriptEngine engine = (JProlScriptEngine) factory.getScriptEngine(
        new MathLibrary(),
        new StringLibrary(),
        new CollectionLibrary()
    );

    // Complex query using multiple libraries
    engine.consult(
        "process_numbers(List, Sum, Desc) :- " +
            "  list_sum_custom(List, Sum), " +
            "  square(Sum, SumSq), " +
            "  str_concat_custom('Sum squared: ', SumSq, Desc)."
    );

    List<Map<String, Object>> results = engine.query("process_numbers([1,2,3,4], S, D).");
    assertFalse(results.isEmpty(), "Complex query should succeed");

    Map<String, Object> solution = results.get(0);
    assertEquals(10L, solution.get("S"), "Sum should be 10");

    String desc = solution.get("D").toString();
    assertTrue(desc.contains("100"), "Description should contain squared sum");

    System.out.println("✓ Complex integration test passed");
    System.out.println("  Sum: " + solution.get("S"));
    System.out.println("  Description: " + desc);
    System.out.println();
  }

  @Test
  @Order(10)
  @DisplayName("Test 10: Factory Methods")
  public void testFactoryMethods() {
    System.out.println("Test 10: Factory Methods");
    System.out.println("------------------------");

    assertEquals("JProl", factory.getEngineName(), "Engine name should be JProl");
    assertEquals("Prolog", factory.getLanguageName(), "Language should be Prolog");
    assertTrue(factory.getExtensions().contains("pl"), "Should support .pl extension");
    assertTrue(factory.getNames().contains("prolog"), "Should have 'prolog' name");

    System.out.println("✓ Engine name: " + factory.getEngineName());
    System.out.println("✓ Language: " + factory.getLanguageName());
    System.out.println("✓ Extensions: " + factory.getExtensions());
    System.out.println("✓ Names: " + factory.getNames());
    System.out.println();
  }

  @AfterAll
  public void tearDown() {
    System.out.println("=== All Tests Completed Successfully ===");
  }

  // ========== Custom Library Implementations ==========

  /**
   * Basic custom library with simple predicates.
   */
  public static class BasicCustomLibrary extends AbstractJProlLibrary {

    public BasicCustomLibrary() {
      super("BasicCustomLibrary");
    }

    @JProlPredicate(
        signature = "hello_world/0",
        determined = true,
        reference = "Print hello world message"
    )
    public static boolean predicateHelloWorld(JProlChoicePoint goal, TermStruct predicate) {
      System.out.println("  [Library Output] Hello from custom JProl library!");
      return true;
    }

    @JProlPredicate(
        signature = "greet/1",
        args = {"+atom"},
        determined = true,
        reference = "Greet someone by name"
    )
    public static boolean predicateGreet(JProlChoicePoint goal, TermStruct predicate) {
      Term name = predicate.getElement(0).findNonVarOrSame();
      System.out.println("  [Library Output] Hello, " + name.getText() + "!");
      return true;
    }
  }

  /**
   * Math library with arithmetic operations.
   */
  public static class MathLibrary extends AbstractJProlLibrary {

    public MathLibrary() {
      super("MathLibrary");
    }

    @JProlPredicate(
        signature = "square/2",
        args = {"+number,?number"},
        determined = true,
        reference = "Calculate square of a number"
    )
    public static boolean predicateSquare(JProlChoicePoint goal, TermStruct predicate) {
      NumericTerm input = predicate.getElement(0).findNonVarOrSame();
      long value = input.toNumber().longValue();
      return predicate.getElement(1).findNonVarOrSame().unifyTo(Terms.newLong(value * value));
    }

    @JProlPredicate(
        signature = "cube/2",
        args = {"+number,?number"},
        determined = true,
        reference = "Calculate cube of a number"
    )
    public static boolean predicateCube(JProlChoicePoint goal, TermStruct predicate) {
      NumericTerm input = predicate.getElement(0).findNonVarOrSame();
      long value = input.toNumber().longValue();
      return predicate.getElement(1).findNonVarOrSame()
          .unifyTo(Terms.newLong(value * value * value));
    }

    @JProlPredicate(
        signature = "factorial_custom/2",
        args = {"+integer,?integer"},
        determined = true,
        reference = "Calculate factorial"
    )
    public static boolean predicateFactorial(JProlChoicePoint goal, TermStruct predicate) {
      NumericTerm input = predicate.getElement(0).findNonVarOrSame();
      int n = input.toNumber().intValue();

      long result = 1;
      for (int i = 2; i <= n; i++) {
        result *= i;
      }

      return predicate.getElement(1).findNonVarOrSame().unifyTo(Terms.newLong(result));
    }

    @JProlPredicate(
        signature = "is_even/1",
        args = {"+integer"},
        determined = true,
        reference = "Check if number is even"
    )
    public static boolean predicateIsEven(JProlChoicePoint goal, TermStruct predicate) {
      NumericTerm input = predicate.getElement(0).findNonVarOrSame();
      long value = input.toNumber().longValue();
      return value % 2 == 0;
    }

    @JProlPredicate(
        signature = "is_odd/1",
        args = {"+integer"},
        determined = true,
        reference = "Check if number is odd"
    )
    public static boolean predicateIsOdd(JProlChoicePoint goal, TermStruct predicate) {
      NumericTerm input = predicate.getElement(0).findNonVarOrSame();
      long value = input.toNumber().longValue();
      return value % 2 != 0;
    }
  }

  /**
   * String manipulation library.
   */
  public static class StringLibrary extends AbstractJProlLibrary {

    public StringLibrary() {
      super("StringLibrary");
    }

    @JProlPredicate(
        signature = "str_upper/2",
        args = {"+atom,?atom"},
        determined = true,
        reference = "Convert string to uppercase"
    )
    public static boolean predicateStrUpper(JProlChoicePoint goal, TermStruct predicate) {
      Term input = predicate.getElement(0).findNonVarOrSame();
      String text = input.getText();
      return predicate.getElement(1).findNonVarOrSame().unifyTo(Terms.newAtom(text.toUpperCase()));
    }

    @JProlPredicate(
        signature = "str_lower/2",
        args = {"+atom,?atom"},
        determined = true,
        reference = "Convert string to lowercase"
    )
    public static boolean predicateStrLower(JProlChoicePoint goal, TermStruct predicate) {
      Term input = predicate.getElement(0).findNonVarOrSame();
      String text = input.getText();
      return predicate.getElement(1).findNonVarOrSame().unifyTo(Terms.newAtom(text.toLowerCase()));
    }

    @JProlPredicate(
        signature = "str_length/2",
        args = {"+atom,?integer"},
        determined = true,
        reference = "Get length of string"
    )
    public static boolean predicateStrLength(JProlChoicePoint goal, TermStruct predicate) {
      Term input = predicate.getElement(0).findNonVarOrSame();
      String text = input.getText();
      return predicate.getElement(1).findNonVarOrSame().unifyTo(Terms.newLong(text.length()));
    }

    @JProlPredicate(
        signature = "str_reverse/2",
        args = {"+atom,?atom"},
        determined = true,
        reference = "Reverse a string"
    )
    public static boolean predicateStrReverse(JProlChoicePoint goal, TermStruct predicate) {
      Term input = predicate.getElement(0).findNonVarOrSame();
      String text = input.getText();
      String reversed = new StringBuilder(text).reverse().toString();
      return predicate.getElement(1).findNonVarOrSame().unifyTo(Terms.newAtom(reversed));
    }

    @JProlPredicate(
        signature = "str_concat_custom/3",
        args = {"+atom,+atom,?atom"},
        determined = true,
        reference = "Concatenate two strings"
    )
    public static boolean predicateStrConcat(JProlChoicePoint goal, TermStruct predicate) {
      Term input1 = predicate.getElement(0).findNonVarOrSame();
      Term input2 = predicate.getElement(1).findNonVarOrSame();
      String result = input1.getText() + input2.getText();
      return predicate.getElement(2).findNonVarOrSame().unifyTo(Terms.newAtom(result));
    }
  }

  /**
   * Date/Time library
   */
  public static class DateTimeLibrary extends AbstractJProlLibrary {

    public DateTimeLibrary() {
      super("DateTimeLibrary");
    }

    @JProlPredicate(
        signature = "current_timestamp/1",
        args = {"?integer"},
        determined = true,
        reference = "Get current Unix timestamp"
    )
    public static boolean predicateCurrentTimestamp(JProlChoicePoint goal, TermStruct predicate) {
      final Term term = predicate.getElement(0).findNonVarOrSame();
      return term.unifyTo(Terms.newLong(System.currentTimeMillis() / 1000L));
    }

    @JProlPredicate(
        signature = "current_date/3",
        args = {"?integer,?integer,?integer"},
        determined = true,
        reference = "Get current date as year, month, day"
    )
    public static boolean predicateCurrentDate(JProlChoicePoint goal, TermStruct predicate) {
      final Calendar cal = Calendar.getInstance();
      return
          predicate.getElement(0).findNonVarOrSame().unifyTo(Terms.newLong(cal.get(Calendar.YEAR)))
              && predicate.getElement(1).findNonVarOrSame()
              .unifyTo(Terms.newLong(cal.get(Calendar.MONTH) + 1))
              && predicate.getElement(2).findNonVarOrSame()
              .unifyTo(Terms.newLong(cal.get(Calendar.DAY_OF_MONTH)));
    }
  }

  /**
   * Collection utilities library
   */
  public static class CollectionLibrary extends AbstractJProlLibrary {

    public CollectionLibrary() {
      super("CollectionLibrary");
    }

    @JProlPredicate(
        signature = "list_unique/2",
        args = {"+list,?list"},
        determined = true,
        reference = "Remove duplicates from list"
    )
    public static boolean predicateListUnique(JProlChoicePoint goal, TermStruct predicate) {
      TermList inputList = predicate.getElement(0).findNonVarOrSame();

      Set<String> seen = new LinkedHashSet<>();
      List<Term> uniqueTerms = new ArrayList<>();

      TermList current = inputList;
      while (!current.isNullList()) {
        Term head = current.getHead();
        String repr = head.toString();
        if (seen.add(repr)) {
          uniqueTerms.add(head);
        }

        Term tail = current.getTail();
        if (tail instanceof TermList) {
          current = (TermList) tail;
        } else {
          break;
        }
      }

      // Build result list
      return predicate.getElement(1).findNonVarOrSame().unifyTo(TermList.asList(uniqueTerms));
    }

    @JProlPredicate(
        signature = "list_sum_custom/2",
        args = {"+list,?number"},
        determined = true,
        reference = "Calculate sum of numeric list"
    )
    public static boolean predicateListSum(JProlChoicePoint goal, TermStruct predicate) {
      TermList inputList = predicate.getElement(0).findNonVarOrSame();

      long sum = 0;
      TermList current = inputList;

      while (!current.isNullList()) {
        Term head = current.getHead().findNonVarOrSame();
        if (head instanceof NumericTerm) {
          sum += head.toNumber().longValue();
        }

        Term tail = current.getTail().findNonVarOrSame();
        if (tail instanceof TermList) {
          current = (TermList) tail;
        } else {
          break;
        }
      }

      return predicate.getElement(1).findNonVarOrSame().unifyTo(Terms.newLong(sum));
    }
  }
}