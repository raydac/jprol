package com.igormaznitsa.jprol.libs;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import com.igormaznitsa.jprol.it.AbstractJProlTest;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;
import com.igormaznitsa.jprol.logic.io.IoResourceProvider;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

class JProlIoLibraryTest extends AbstractJProlTest {
  private static final IoResourceProvider FAKE_IO_PROVIDER = new IoResourceProvider() {
    @Override
    public Reader findReader(final JProlContext context, final String readerId) {
      if (readerId.equals("fake_in")) {
        return new StringReader("fake string");
      } else {
        return null;
      }
    }

    @Override
    public Writer findWriter(final JProlContext context, final String writerId,
                             final boolean append) {
      if (writerId.equals("fake_out")) {
        return new StringWriter();
      } else {
        return null;
      }
    }
  };

  @Test
  void testSeeing1() {
    final JProlContext context = makeTestContext();
    context.addIoResourceProvider(FAKE_IO_PROVIDER);

    JProlChoicePoint point = prepareGoal("see(fake_in), seeing(X).", context);
    assertNotNull(point.prove());
    assertEquals("fake_in", point.findVar("X").get().getValue().getText());

    point = prepareGoal("see(fake_in), current_input(X).", context);
    assertNotNull(point.prove());
    assertEquals("fake_in", point.findVar("X").get().getValue().getText());
  }

  @Test
  void testTelling1() {
    final JProlContext context = makeTestContext();
    context.addIoResourceProvider(FAKE_IO_PROVIDER);

    JProlChoicePoint point = prepareGoal("tell(fake_out), telling(X).", context);
    assertNotNull(point.prove());
    assertEquals("fake_out", point.findVar("X").get().getValue().getText());

    point = prepareGoal("tell(fake_out), current_output(X).", context);
    assertNotNull(point.prove());
    assertEquals("fake_out", point.findVar("X").get().getValue().getText());
  }

  @Test
  @Disabled
  void testSeek1() {
    //TODO seek/2
    //[(seek(my_file,3),at(my_file,X)),in(my_file),[[X <-- 3]]].
    //[(seek(my_file,eof),at(my_file,X)),in(my_file),[[X <-- eof]]].
    //[(seek(my_file,3),get_char(X,my_file)),in(my_file),[[X <-- e]]].
  }

}