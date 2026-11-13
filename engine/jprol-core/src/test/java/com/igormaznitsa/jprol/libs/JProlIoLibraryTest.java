package com.igormaznitsa.jprol.libs;

import static java.util.Arrays.asList;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.igormaznitsa.jprol.exceptions.ProlExistenceErrorException;
import com.igormaznitsa.jprol.it.AbstractJProlTest;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;
import com.igormaznitsa.jprol.logic.io.IoResourceProvider;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import java.util.HashMap;
import java.util.Map;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

class JProlIoLibraryTest extends AbstractJProlTest {

  private static final IoResourceProvider FAKE_IO_PROVIDER = new IoResourceProvider() {
    @Override
    public Reader findReader(final JProlContext context, final String readerId) {
      if ("fake_in".equals(readerId)) {
        return new StringReader("fake string");
      }
      return null;
    }

    @Override
    public Writer findWriter(
        final JProlContext context,
        final String writerId,
        final boolean append
    ) {
      if ("fake_out".equals(writerId)) {
        return new StringWriter();
      }
      return null;
    }
  };

  @Test
  void testSeeing1() {
    try (final JProlContext context = this.makeTestContext(FAKE_IO_PROVIDER)) {
      JProlChoicePoint point = this.prepareGoal("see(fake_in), seeing(X).", context);
      assertNotNull(point.prove());
      assertEquals("fake_in", point.findVar("X").orElseThrow().getValue().getText());

      point = this.prepareGoal("see(fake_in), current_input(X).", context);
      assertNotNull(point.prove());
      assertEquals("fake_in", point.findVar("X").orElseThrow().getValue().getText());
    }
  }

  @Test
  void testTelling1() {
    try (final JProlContext context = this.makeTestContext(FAKE_IO_PROVIDER)) {
      JProlChoicePoint point = this.prepareGoal("tell(fake_out), telling(X).", context);
      assertNotNull(point.prove());
      assertEquals("fake_out", point.findVar("X").orElseThrow().getValue().getText());

      point = this.prepareGoal("tell(fake_out), current_output(X).", context);
      assertNotNull(point.prove());
      assertEquals("fake_out", point.findVar("X").orElseThrow().getValue().getText());
    }
  }

  @Test
  void testOutputStreamsWriteWritelnTabPutFormatAndTold() {
    final Map<String, StringWriter> sinks = new HashMap<>();
    final IoResourceProvider provider = new IoResourceProvider() {
      @Override
      public Writer findWriter(
          final JProlContext context,
          final String writerId,
          final boolean append
      ) {
        return sinks.computeIfAbsent(writerId, key -> new StringWriter());
      }
    };

    try (final JProlContext context = this.makeTestContext(provider)) {
      final JProlChoicePoint cp = context.makeChoicePoint(
          "tell(out_main),"
              + " write(header), tab(2), put(65), nl,"
              + " writeln(pair(1, 2)),"
              + " format('sum=~d~n', [42]),"
              + " told."
      );
      assertNotNull(cp.prove());
      assertNull(cp.prove());
      assertEquals("header  A\npair(1,2)\nsum=42\n", sinks.get("out_main").toString());
    }
  }

  @Test
  void testInputStreamsReadGetGet0ReadLineAndSeen() {
    final Map<String, String> inputs = Map.of(
        "terms_in", "fact(10). [a, b]. ",
        "chars_in", "  \n\tZ!",
        "lines_in", "first line\nsecond line"
    );
    final IoResourceProvider provider = new IoResourceProvider() {
      @Override
      public Reader findReader(final JProlContext context, final String readerId) {
        final String text = inputs.get(readerId);
        return text == null ? null : new StringReader(text);
      }
    };

    try (final JProlContext context = this.makeTestContext(provider)) {
      final JProlChoicePoint readTerms = context.makeChoicePoint(
          "see(terms_in), read(T1), read(T2), read(T3), seen, R = [T1, T2, T3]."
      );
      assertNotNull(readTerms.prove());
      assertEquals("[fact(10),['a','b'],'end_of_file']", getVarAsText(readTerms, "R"));

      final JProlChoicePoint readChars = context.makeChoicePoint(
          "see(chars_in), get0(FirstRaw), get(FirstNonBlank), get0(Excl), get0(Eof), seen,"
              + " R = [FirstRaw, FirstNonBlank, Excl, Eof]."
      );
      assertNotNull(readChars.prove());
      assertEquals("[32,90,33,-1]", getVarAsText(readChars, "R"));

      final JProlChoicePoint readLines = context.makeChoicePoint(
          "see(lines_in), read_line_to_string(_, L1), read_line_to_string(_, L2), seen,"
              + " R = L1 - L2."
      );
      assertNotNull(readLines.prove());
      assertEquals("'first line' - 'second line'", getVarAsText(readLines, "R"));

      assertThrows(
          ProlExistenceErrorException.class,
          () -> context.makeChoicePoint("see(unknown_stream_id).").prove()
      );
    }
  }

  @Test
  void testConsultPredicateLoadsResourceListAndSingleUri() {
    final Map<String, String> modules = Map.of(
        "mod_edges", "edge(a, b). edge(b, c).",
        "mod_rules", "reach(X, Y) :- edge(X, Y). reach(X, Z) :- edge(X, Y), reach(Y, Z)."
    );
    final IoResourceProvider provider = new IoResourceProvider() {
      @Override
      public Reader findReader(final JProlContext context, final String readerId) {
        final String src = modules.get(readerId);
        return src == null ? null : new StringReader(src);
      }
    };

    try (final JProlContext context = this.makeTestContext(provider)) {
      assertNotNull(context.makeChoicePoint("consult([mod_edges, mod_rules]).").prove());
      final JProlChoicePoint cp = context.makeChoicePoint("findall(T, reach(a, T), Ts).");
      assertNotNull(cp.prove());
      assertEquals("['b','c']", getVarAsText(cp, "Ts"));
    }
  }

  @Test
  void testTimePredicatePreservesBacktrackingAndWritesElapsedMillis() {
    final StringWriter out = new StringWriter();
    final IoResourceProvider provider = new IoResourceProvider() {
      @Override
      public Writer findWriter(
          final JProlContext context,
          final String writerId,
          final boolean append
      ) {
        return out;
      }
    };

    try (final JProlContext context = this.makeTestContext(provider)) {
      context.consult(new StringReader("v(10). v(20)."));
      final JProlChoicePoint cp = context.makeChoicePoint("tell(log), time(v(X)).");
      assertEquals(asList("10", "20"), asList(
          cp.prove() != null ? getVarAsText(cp, "X") : "none",
          cp.prove() != null ? getVarAsText(cp, "X") : "none"
      ));
      assertNull(cp.prove());
      assertTrue(out.toString().contains("ms"));
    }
  }

  @Test
  @Disabled
  void testSeek1() {
  }
}
