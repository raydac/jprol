package com.igormaznitsa.jprol.utils;

import static com.igormaznitsa.jprol.data.Terms.newAtom;
import static com.igormaznitsa.jprol.data.Terms.newDouble;
import static com.igormaznitsa.jprol.data.Terms.newLong;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.igormaznitsa.jprol.data.Term;
import java.util.stream.Stream;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * JUnit 5 test suite for PrologFormatConverter
 */
@DisplayName("Prolog Format Converter Tests")
class PrologFormatConverterTest {

  private static Stream<Arguments> provideFormatTestCases() {
    return Stream.of(
        Arguments.of("~a", new Term[] {newAtom("test")}, "test"),
        Arguments.of("~d", new Term[] {newLong(123)}, "123"),
        Arguments.of("~a ~d", new Term[] {newAtom("Number"), newLong(42)}, "Number 42"),
        Arguments.of("Value: ~d", new Term[] {newLong(-99)}, "Value: -99"),
        Arguments.of("~~test", new Term[] {}, "~test"),
        Arguments.of("~2f", new Term[] {newDouble(3.14159)}, "3.14"),
        Arguments.of("~8r", new Term[] {newLong(8)}, "10"),
        Arguments.of("~16r", new Term[] {newLong(16)}, "10")
    );
  }

  @ParameterizedTest
  @DisplayName("Should convert various Prolog formats")
  @CsvSource({
      "~a, %1$s",
      "~d, %1$s",
      "~f, %1$.6f",
      "~n, %n",
      "~~, ~",
      "~a~d, %1$s%2$s",
      "'Hello ~a!', 'Hello %1$s!'"
  })
  void testParameterizedConversions(String prologFormat, String expectedJavaFormat) {
    String result = PrologFormatConverter.convertFormat(prologFormat);
    assertEquals(expectedJavaFormat, result);
  }

  @ParameterizedTest
  @DisplayName("Should format with various argument types")
  @MethodSource("provideFormatTestCases")
  void testParameterizedFormatting(String prologFormat, Term[] args, String expected) {
    String result = PrologFormatConverter.format(prologFormat, args);
    assertEquals(expected, result);
  }

  @Nested
  @DisplayName("Basic Format Conversion Tests")
  class BasicConversionTests {

    @Test
    @DisplayName("Should convert atom directive ~a to %s")
    void testAtomConversion() {
      String result = PrologFormatConverter.convertFormat("~a");
      assertEquals("%1$s", result);
    }

    @Test
    @DisplayName("Should convert decimal directive ~d to %d")
    void testDecimalConversion() {
      String result = PrologFormatConverter.convertFormat("~d");
      assertEquals("%1$s", result);
    }

    @Test
    @DisplayName("Should convert write directive ~w to %s")
    void testWriteConversion() {
      String result = PrologFormatConverter.convertFormat("~w");
      assertEquals("%1$s", result);
    }

    @Test
    @DisplayName("Should convert quoted directive ~q to %s")
    void testQuotedConversion() {
      String result = PrologFormatConverter.convertFormat("~q");
      assertEquals("%1$s", result);
    }

    @Test
    @DisplayName("Should convert string directive ~s to %s")
    void testStringConversion() {
      String result = PrologFormatConverter.convertFormat("~s");
      assertEquals("%1$s", result);
    }

    @Test
    @DisplayName("Should convert character directive ~c to %c")
    void testCharacterConversion() {
      String result = PrologFormatConverter.convertFormat("~c");
      assertEquals("%1$s", result);
    }
  }

  @Nested
  @DisplayName("Numeric Format Conversion Tests")
  class NumericConversionTests {

    @Test
    @DisplayName("Should convert float directive ~f to %f")
    void testFloatConversion() {
      String result = PrologFormatConverter.convertFormat("~f");
      assertEquals("%1$.6f", result);
    }

    @Test
    @DisplayName("Should convert float with precision ~2f to %.2f")
    void testFloatWithPrecision() {
      String result = PrologFormatConverter.convertFormat("~2f");
      assertEquals("%1$.2f", result);
    }

    @Test
    @DisplayName("Should convert float with precision ~3f to %.3f")
    void testFloatWithPrecision3() {
      String result = PrologFormatConverter.convertFormat("~3f");
      assertEquals("%1$.3f", result);
    }

    @Test
    @DisplayName("Should convert binary radix ~2r to %s")
    void testBinaryRadix() {
      String result = PrologFormatConverter.convertFormat("~2r");
      assertEquals("%1$s", result);
    }

    @Test
    @DisplayName("Should convert octal radix ~8r to %o")
    void testOctalRadix() {
      String result = PrologFormatConverter.convertFormat("~8r");
      assertEquals("%1$o", result);
    }

    @Test
    @DisplayName("Should convert hex radix ~16r to %x")
    void testHexRadix() {
      String result = PrologFormatConverter.convertFormat("~16r");
      assertEquals("%1$x", result);
    }

    @Test
    @DisplayName("Should convert exponential directive ~e to %e")
    void testExponentialConversion() {
      String result = PrologFormatConverter.convertFormat("~e");
      assertEquals("%1$.6e", result);
    }

    @Test
    @DisplayName("Should convert exponential with precision ~2e to %.2e")
    void testExponentialWithPrecision() {
      String result = PrologFormatConverter.convertFormat("~2e");
      assertEquals("%1$.2e", result);
    }

    @Test
    @DisplayName("Should convert general float directive ~g to %g")
    void testGeneralFloatConversion() {
      String result = PrologFormatConverter.convertFormat("~g");
      assertEquals("%1$.6g", result);
    }

    @Test
    @DisplayName("Should convert general float with precision ~4g to %.4g")
    void testGeneralFloatWithPrecision() {
      String result = PrologFormatConverter.convertFormat("~4g");
      assertEquals("%1$.4g", result);
    }
  }

  @Nested
  @DisplayName("Escape Sequence Tests")
  class EscapeSequenceTests {

    @Test
    @DisplayName("Should convert newline ~n to %n")
    void testNewlineConversion() {
      String result = PrologFormatConverter.convertFormat("~n");
      assertEquals("%n", result);
    }

    @Test
    @DisplayName("Should convert tab ~t to \\t")
    void testTabConversion() {
      String result = PrologFormatConverter.convertFormat("~t");
      assertEquals("\t", result);
    }

    @Test
    @DisplayName("Should convert literal tilde ~~ to ~")
    void testLiteralTildeConversion() {
      String result = PrologFormatConverter.convertFormat("~~");
      assertEquals("~", result);
    }

    @Test
    @DisplayName("Should escape % in format string")
    void testPercentEscaping() {
      String result = PrologFormatConverter.convertFormat("Discount: ~d% off");
      assertEquals("Discount: %1$s%% off", result);
    }
  }

  @Nested
  @DisplayName("Multiple Argument Tests")
  class MultipleArgumentTests {

    @Test
    @DisplayName("Should handle multiple arguments with correct indices")
    void testMultipleArguments() {
      String result = PrologFormatConverter.convertFormat("~a ~d ~f");
      assertEquals("%1$s %2$s %3$.6f", result);
    }

    @Test
    @DisplayName("Should handle mixed directives")
    void testMixedDirectives() {
      String result = PrologFormatConverter.convertFormat("Name: ~a, Age: ~d, Score: ~2f");
      assertEquals("Name: %1$s, Age: %2$s, Score: %3$.2f", result);
    }

    @Test
    @DisplayName("Should handle ignore directive ~i")
    void testIgnoreDirective() {
      String result = PrologFormatConverter.convertFormat("~a~i~a");
      // After conversion, ignored arg is removed from output, but indices continue
      // The convertArguments method will handle skipping the actual argument
      assertEquals("%1$s%2$s", result);
    }
  }

  @Nested
  @DisplayName("Complete Formatting Tests")
  class CompleteFormattingTests {

    @Test
    @DisplayName("Should format simple atom")
    void testFormatAtom() {
      String result = PrologFormatConverter.format("Hello ~a!", newAtom("World"));
      assertEquals("Hello World!", result);
    }

    @Test
    @DisplayName("Should format integer")
    void testFormatInteger() {
      String result = PrologFormatConverter.format("Number: ~d", newLong(42));
      assertEquals("Number: 42", result);
    }

    @Test
    @DisplayName("Should format float with precision")
    void testFormatFloatWithPrecision() {
      String result = PrologFormatConverter.format("Pi: ~2f", newDouble(3.14159));
      assertEquals("Pi: 3.14", result);
    }

    @Test
    @DisplayName("Should format float with 3 decimal precision")
    void testFormatFloatWithPrecision3() {
      String result = PrologFormatConverter.format("Pi: ~3f", newDouble(3.14159));
      assertEquals("Pi: 3.142", result);
    }

    @Test
    @DisplayName("Should format multiple arguments")
    void testFormatMultipleArgs() {
      String result =
          PrologFormatConverter.format("~a is ~d years old", newAtom("Alice"), newLong(30));
      assertEquals("Alice is 30 years old", result);
    }

    @Test
    @DisplayName("Should format with newline")
    void testFormatWithNewline() {
      String result = PrologFormatConverter.format("Line1~nLine2");
      assertEquals("Line1" + System.lineSeparator() + "Line2", result);
    }

    @Test
    @DisplayName("Should format with tab")
    void testFormatWithTab() {
      String result = PrologFormatConverter.format("Col1~tCol2");
      assertEquals("Col1\tCol2", result);
    }

    @Test
    @DisplayName("Should handle ignored arguments")
    void testFormatWithIgnore() {
      String result =
          PrologFormatConverter.format("First: ~a~i Third: ~a", newAtom("one"), newAtom("two"),
              newAtom("three"));
      assertEquals("First: one Third: three", result);
    }
  }

  @Nested
  @DisplayName("Radix Conversion Tests")
  class RadixConversionTests {

    @Test
    @DisplayName("Should convert integer to binary string")
    void testBinaryConversion() {
      String result = PrologFormatConverter.format("Binary: ~2r", newLong(15));
      assertEquals("Binary: 1111", result);
    }

    @Test
    @DisplayName("Should convert integer to octal")
    void testOctalConversion() {
      String result = PrologFormatConverter.format("Octal: ~8r", newLong(64));
      assertEquals("Octal: 100", result);
    }

    @Test
    @DisplayName("Should convert integer to hexadecimal")
    void testHexConversion() {
      String result = PrologFormatConverter.format("Hex: ~16r", newLong(255));
      assertEquals("Hex: ff", result);
    }

    @Test
    @DisplayName("Should handle zero in binary")
    void testBinaryZero() {
      String result = PrologFormatConverter.format("~2r", newLong(0));
      assertEquals("0", result);
    }

    @Test
    @DisplayName("Should handle large numbers in binary")
    void testBinaryLarge() {
      String result = PrologFormatConverter.format("~2r", newLong(255));
      assertEquals("11111111", result);
    }
  }

  @Nested
  @DisplayName("Argument Conversion Tests")
  class ArgumentConversionTests {

    @Test
    @DisplayName("Should convert arguments for binary format")
    void testBinaryArgumentConversion() {
      Object[] converted = PrologFormatConverter.convertArguments("~2r", new Term[] {newLong(15)});
      assertEquals(1, converted.length);
      assertEquals("1111", converted[0]);
    }

    @Test
    @DisplayName("Should handle mixed argument types")
    void testMixedArgumentConversion() {
      Object[] converted = PrologFormatConverter.convertArguments(
          "~a ~d ~2r",
          new Term[] {newAtom("text"), newLong(42), newLong(8)}
      );
      assertEquals(3, converted.length);
      assertEquals("text", converted[0]);
      assertEquals("42", converted[1]);
      assertEquals("1000", converted[2]);
    }

    @Test
    @DisplayName("Should skip ignored arguments")
    void testIgnoredArgumentConversion() {
      Object[] converted = PrologFormatConverter.convertArguments(
          "~a~i~a",
          new Term[] {newAtom("first"), newAtom("ignored"), newAtom("third")}
      );
      assertEquals(2, converted.length);
      assertEquals("first", converted[0]);
      assertEquals("third", converted[1]);
    }

    @Test
    @DisplayName("Should handle non-numeric arguments for binary")
    void testNonNumericBinaryConversion() {
      Object[] converted =
          PrologFormatConverter.convertArguments("~2r", new Term[] {newAtom("not a number")});
      assertEquals(1, converted.length);
      assertEquals("not a number", converted[0]);
    }
  }

  @Nested
  @DisplayName("Edge Cases and Error Handling")
  class EdgeCaseTests {

    @Test
    @DisplayName("Should handle null format string")
    void testNullFormat() {
      String result = PrologFormatConverter.convertFormat(null);
      assertNull(result);
    }

    @Test
    @DisplayName("Should handle empty format string")
    void testEmptyFormat() {
      String result = PrologFormatConverter.convertFormat("");
      assertEquals("", result);
    }

    @Test
    @DisplayName("Should handle format with no directives")
    void testNoDirectives() {
      String result = PrologFormatConverter.convertFormat("Plain text");
      assertEquals("Plain text", result);
    }

    @Test
    @DisplayName("Should handle null arguments")
    void testNullArguments() {
      Object[] converted = PrologFormatConverter.convertArguments("~a", null);
      assertNotNull(converted);
      assertEquals(0, converted.length);
    }

    @Test
    @DisplayName("Should handle empty arguments array")
    void testEmptyArguments() {
      Object[] converted = PrologFormatConverter.convertArguments("~a", new Term[] {});
      assertNotNull(converted);
      assertEquals(0, converted.length);
    }

    @Test
    @DisplayName("Should handle tilde at end of string")
    void testTildeAtEnd() {
      String result = PrologFormatConverter.convertFormat("Test~");
      assertEquals("Test~", result);
    }

    @Test
    @DisplayName("Should handle consecutive directives")
    void testConsecutiveDirectives() {
      String result = PrologFormatConverter.convertFormat("~a~d~f");
      assertEquals("%1$s%2$s%3$.6f", result);
    }

    @Test
    @DisplayName("Should handle literal percent signs")
    void testLiteralPercent() {
      String converted = PrologFormatConverter.convertFormat("100% complete");
      assertEquals("100%% complete", converted);
      String result = PrologFormatConverter.format("100% complete");
      assertEquals("100% complete", result);
    }

    @Test
    @DisplayName("Should handle zero as argument")
    void testZeroArgument() {
      String result = PrologFormatConverter.format("Value: ~d", newLong(0));
      assertEquals("Value: 0", result);
    }

    @Test
    @DisplayName("Should handle negative numbers")
    void testNegativeNumber() {
      String result = PrologFormatConverter.format("Negative: ~d", newLong(-42));
      assertEquals("Negative: -42", result);
    }
  }

  @Nested
  @DisplayName("Integration Tests")
  class IntegrationTests {

    @Test
    @DisplayName("Should handle complex mixed format")
    void testComplexFormat() {
      String result = PrologFormatConverter.format(
          "User ~a (ID: ~d) scored ~2f%% on test ~16r",
          newAtom("Alice"), newLong(1234), newDouble(95.67), newLong(255)
      );
      assertTrue(result.contains("Alice"));
      assertTrue(result.contains("1234"));
      assertTrue(result.contains("95.67"));
      assertTrue(result.contains("ff"));
      assertTrue(result.contains("%%"));
    }

    @Test
    @DisplayName("Should handle format with all escape sequences")
    void testAllEscapeSequences() {
      String result = PrologFormatConverter.format(
          "Line1~nTab~there~n~~tilde"
      );
      assertTrue(result.contains(System.lineSeparator()));
      assertTrue(result.contains("\t"));
      assertTrue(result.contains("~tilde"));
    }

    @Test
    @DisplayName("Should format table-like output")
    void testTableFormat() {
      String format = "Name: ~a~tAge: ~d~tScore: ~2f";
      String result =
          PrologFormatConverter.format(format, newAtom("Bob"), newLong(25), newDouble(88.5));
      assertTrue(result.contains("Name: Bob"));
      assertTrue(result.contains("Age: 25"));
      assertTrue(result.contains("Score: 88.50"));
    }

    @Test
    @DisplayName("Should handle character code formatting")
    void testCharacterCode() {
      String result = PrologFormatConverter.format("Char: ~c", newLong(65));
      assertEquals("Char: A", result);
    }

    @Test
    @DisplayName("Should handle multiple radix conversions in one format")
    void testMultipleRadixConversions() {
      String result = PrologFormatConverter.format(
          "Dec: ~d, Bin: ~2r, Oct: ~8r, Hex: ~16r",
          newLong(10), newLong(10), newLong(10), newLong(10)
      );
      assertTrue(result.contains("Dec: 10"));
      assertTrue(result.contains("Bin: 1010"));
      assertTrue(result.contains("Oct: 12"));
      assertTrue(result.contains("Hex: a"));
    }

    @Test
    @DisplayName("Should handle scientific notation")
    void testScientificNotation() {
      String result = PrologFormatConverter.format("Value: ~2e", newDouble(12345.6789));
      assertTrue(result.contains("1.23e+04") || result.contains("1.23E+04"));
    }

    @Test
    @DisplayName("Should handle general float format")
    void testGeneralFloatFormat() {
      String result = PrologFormatConverter.format("Value: ~g", newDouble(0.00012345));
      assertFalse(result.isEmpty());
    }
  }
}