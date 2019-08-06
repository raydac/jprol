package com.igormaznitsa.jprol.libs;

import com.igormaznitsa.jprol.it.AbstractJProlTest;
import com.igormaznitsa.jprol.logic.ChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;
import com.igormaznitsa.jprol.logic.io.IoResourceProvider;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

class JProlIoLibraryTest extends AbstractJProlTest {
  @Test
  void testSeeing1() {
    final JProlContext context = makeTestContext();
    context.addIoResourceProvider(Mockito.mock(IoResourceProvider.class));
    final ChoicePoint point = prepareGoal("seeing(X).", context);
    assertNotNull(point.next());
    assertEquals("[]", point.getVarForName("X").getValue().toString());
  }

  @Test
  @Disabled
  void testSeek1() throws Exception {
    //TODO seek/2
    //[(seek(my_file,3),at(my_file,X)),in(my_file),[[X <-- 3]]].
    //[(seek(my_file,eof),at(my_file,X)),in(my_file),[[X <-- eof]]].
    //[(seek(my_file,3),get_char(X,my_file)),in(my_file),[[X <-- e]]].
  }

  @Test
  @Disabled
  void testCurrentOutput1() throws Exception {
    //TODO current_output/1
    //[exists(current_output/1), success].
  }

}