package com.igormaznitsa.jprol.libs;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.fail;

import com.igormaznitsa.jprol.exceptions.ProlDomainErrorException;
import com.igormaznitsa.jprol.exceptions.ProlTypeErrorException;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

class JProlJsonLibraryTest extends AbstractJProlTest {
  @Test
  void testJson_ok() {
    assertToJson("json([true=@true, false=@false, null=@null])",
        "{\"true\":true,\"false\":false,\"null\":null}");
    assertToJson("json([name='Bob', children=['Mary', 'John'], age=42, married= @(true)])",
        "{\"name\":\"Bob\",\"children\":[\"Mary\",\"John\"],\"age\":42,\"married\":true}");
    assertToJson(
        "json([ name='Demo term',created=json([day= @null, month='December', year=2007]),confirmed= @true,members=[1, 2, 3]])",
        "{\"name\":\"Demo term\",\"created\":{\"day\":null,\"month\":\"December\",\"year\":2007},\"confirmed\":true,\"members\":[1,2,3]}");
  }

  @Test
  void testJson_noUnify() {
    JProlContext context = prepareContext("", new JProlJsonLibrary());
    JProlChoicePoint cp = new JProlChoicePoint("X = 1,  to_json(json([1,2,3]), X).", context);
    assertNull(cp.prove());
  }

  @Test
  void testJson_errorData() {
    assertException("[1,2,3]", ProlTypeErrorException.class);
    assertException("json()", ProlDomainErrorException.class);
    assertException("jsonn([])", ProlDomainErrorException.class);
    assertException("json([1,2,3])", ProlDomainErrorException.class);
    assertException("json([a=1,b=2,c=json([a=1,json([@null])])])", ProlDomainErrorException.class);
    assertException("json([a=1,b=2,c=json([a=1,jsonn([@null])])])", ProlDomainErrorException.class);
    assertException("json([a=1,b=2,c=json([a=1,json([l=@nulll])])])",
        ProlDomainErrorException.class);
    assertException("json([a=1,b=2,c=json([a=1,jsonn([l=@nulll])])])",
        ProlDomainErrorException.class);
  }

  private void assertException(final String term,
                               final Class<? extends Throwable> expectedException) {
    final JProlContext context = prepareContext("", new JProlJsonLibrary());
    final JProlChoicePoint cp =
        new JProlChoicePoint(String.format("to_json(%s, X).", term), context);
    Assertions.assertThrowsExactly(expectedException, () -> {
      cp.prove();
      fail("Must throw exception");
    });
  }

  private void assertToJson(final String term, final String expectedJson) {
    JProlContext context = prepareContext("", new JProlJsonLibrary());
    JProlChoicePoint cp = new JProlChoicePoint(String.format("to_json(%s, X).", term), context);
    assertNotNull(cp.prove());
    assertEquals(
        expectedJson,
        cp.findVar("X").get().getValue().getText());
  }
}