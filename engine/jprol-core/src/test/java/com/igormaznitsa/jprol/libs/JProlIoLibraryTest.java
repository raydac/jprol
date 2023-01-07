package com.igormaznitsa.jprol.libs;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import com.igormaznitsa.jprol.it.AbstractJProlTest;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;
import com.igormaznitsa.jprol.logic.io.IoResourceProvider;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

class JProlIoLibraryTest extends AbstractJProlTest {
  @Test
  void testSeeing1() {
    final JProlContext context = makeTestContext();
    context.addIoResourceProvider(Mockito.mock(IoResourceProvider.class));
    final JProlChoicePoint point = prepareGoal("seeing(X).", context);
    assertNotNull(point.prove());
    assertEquals("[]", point.findVar("X").get().getValue().toString());
  }

  @Test
  @Disabled
  void testSeek1() {
    //TODO seek/2
    //[(seek(my_file,3),at(my_file,X)),in(my_file),[[X <-- 3]]].
    //[(seek(my_file,eof),at(my_file,X)),in(my_file),[[X <-- eof]]].
    //[(seek(my_file,3),get_char(X,my_file)),in(my_file),[[X <-- e]]].
  }

  @Test
  @Disabled
  void testCurrentOutput1() {
    //TODO current_output/1
    //[exists(current_output/1), success].
  }

}