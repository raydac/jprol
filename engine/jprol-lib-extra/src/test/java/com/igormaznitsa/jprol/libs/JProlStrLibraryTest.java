package com.igormaznitsa.jprol.libs;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrowsExactly;

import com.igormaznitsa.jprol.exceptions.ProlDomainErrorException;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;
import org.junit.jupiter.api.Test;

class JProlStrLibraryTest extends AbstractJProlTest {

  @Test
  void testStrFormat() {
    assertStrFormat("'Hello %s'", "[world]", "Hello world");
    assertStrFormat("'Hello %s %d'", "[world,3345]", "Hello world 3345");
    assertStrFormat("'Hello %s %d %f'", "[world,3345,123.456]", "Hello world 3345 123.456000");
    assertThrowsExactly(ProlDomainErrorException.class,
        () -> assertStrFormat("'Hello %s %d %f %s'", "[world,3345,123.456]",
            "Hello world 3345 123.456000"));
    assertThrowsExactly(ProlDomainErrorException.class,
        () -> assertStrFormat("'Hello %d'", "[world]", "Hello world 3345 123.456000"));
  }

  private void assertStrFormat(final String template, final String args, final String expected) {
    final JProlContext context = makeTestContext();
    final String clause = String.format("str_format(%s,%s,X).", template, args);
    final JProlChoicePoint cp = new JProlChoicePoint(clause, context);
    assertNotNull(cp.prove());
    assertEquals(expected, cp.findVar("X").get().getValue().getText());
  }

  private JProlContext makeTestContext() {
    return this.prepareContext("", new JProlCoreLibrary(), new JProlStrLibrary());
  }


}