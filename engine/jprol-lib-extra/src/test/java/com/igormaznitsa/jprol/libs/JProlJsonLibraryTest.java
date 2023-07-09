package com.igormaznitsa.jprol.libs;

import static com.igormaznitsa.jprol.utils.Utils.escapeSrc;
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
  void test_ToJson_ok() {
    assertToJson("json([true=@true, false=@false, null=@null])",
        "{\"true\":true,\"false\":false,\"null\":null}");
    assertToJson("json([name='Bob', children=['Mary', 'John'], age=42, married= @(true)])",
        "{\"name\":\"Bob\",\"children\":[\"Mary\",\"John\"],\"age\":42,\"married\":true}");
    assertToJson(
        "json([ name='Demo term',created=json([day= @null, month='December', year=2007]),confirmed= @true,members=[1, 2, 3]])",
        "{\"name\":\"Demo term\",\"created\":{\"day\":null,\"month\":\"December\",\"year\":2007},\"confirmed\":true,\"members\":[1,2,3]}");
  }

  @Test
  void test_ToJson_noUnify() {
    JProlContext context = prepareContext("", new JProlJsonLibrary());
    JProlChoicePoint cp = new JProlChoicePoint("X = 1,  to_json(json([a=1,b=2,c=3]), X).", context);
    assertNull(cp.prove());
  }

  @Test
  void test_FromJson_noUnify() {
    JProlContext context = prepareContext("", new JProlJsonLibrary());
    JProlChoicePoint cp =
        new JProlChoicePoint("X = 1,  from_json('{\"hello\":\"woeld\"}', X).", context);
    assertNull(cp.prove());
  }

  @Test
  void test_ToJson_errorData() {
    assertToJsonException("[1,2,3]", ProlTypeErrorException.class);
    assertToJsonException("json()", ProlDomainErrorException.class);
    assertToJsonException("jsonn([])", ProlDomainErrorException.class);
    assertToJsonException("json([1,2,3])", ProlDomainErrorException.class);
    assertToJsonException("json([a=1,b=2,c=json([a=1,json([@null])])])",
        ProlDomainErrorException.class);
    assertToJsonException("json([a=1,b=2,c=json([a=1,jsonn([@null])])])",
        ProlDomainErrorException.class);
    assertToJsonException("json([a=1,b=2,c=json([a=1,json([l=@nulll])])])",
        ProlDomainErrorException.class);
    assertToJsonException("json([a=1,b=2,c=json([a=1,jsonn([l=@nulll])])])",
        ProlDomainErrorException.class);
  }

  @Test
  void test_FromJson_ok() {
    assertFromJson("{\"hello\":3}", "json(['hello' = 3])");
    assertFromJson("{\"hello\":\"world\"}", "json(['hello' = 'world'])");
    assertFromJson("{\"a\":null,\"b\":true,\"c\":false}", "json([a=@null,b=@true,c=@false])");
    assertFromJson("{\"a\":[1,2,3,{\"h\":[3,2,4]},[]],\"b\":[{\"ss\":[33.2,44.11,32323]}]}",
        "json([a=[1,2,3,json([h=[3,2,4]]),[]],b=[json([ss=[33.2,44.11,32323]])]])");
  }

  @Test
  void test_FromJson_error() {
    assertFromJsonException("", ProlDomainErrorException.class);
    assertFromJsonException("aaa", ProlDomainErrorException.class);
    assertFromJsonException("{\"hello\":\"world}", ProlDomainErrorException.class);
    assertFromJsonException("{h:\"sss\"}", ProlDomainErrorException.class);
    assertFromJsonException("{{{{{}}}}}}}}}", ProlDomainErrorException.class);
    assertFromJsonException("{{{{[[[", ProlDomainErrorException.class);
    assertFromJsonException("{\"s\":1,,\"d\":111}", ProlDomainErrorException.class);
    assertFromJsonException("{\"s\":nulll}", ProlDomainErrorException.class);
  }

  private void assertToJsonException(final String term,
                                     final Class<? extends Throwable> expectedException) {
    final JProlContext context = prepareContext("", new JProlJsonLibrary());
    final JProlChoicePoint cp =
        new JProlChoicePoint(String.format("to_json(%s, X).", term), context);
    Assertions.assertThrowsExactly(expectedException, () -> {
      cp.prove();
      fail("Must throw exception");
    });
  }

  private void assertFromJsonException(final String json,
                                       final Class<? extends Throwable> expectedException) {
    final JProlContext context = prepareContext("", new JProlJsonLibrary());
    final JProlChoicePoint cp =
        new JProlChoicePoint(String.format("from_json('%s', X).", json), context);
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

  private void assertFromJson(final String json, final String expectedTerm) {
    JProlContext context = prepareContext("", new JProlJsonLibrary());
    final String prepared = String.format("from_json('%s', X), X=%s.",
        escapeSrc(json), expectedTerm);
    JProlChoicePoint cp = new JProlChoicePoint(prepared, context);
    assertNotNull(cp.prove(), "Can't prove goal or not expected value");
  }
}