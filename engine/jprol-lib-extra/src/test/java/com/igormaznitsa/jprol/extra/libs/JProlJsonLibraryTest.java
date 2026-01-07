package com.igormaznitsa.jprol.extra.libs;

import static com.igormaznitsa.jprol.utils.ProlUtils.escapeSrc;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.fail;

import com.igormaznitsa.jprol.exceptions.ProlDomainErrorException;
import com.igormaznitsa.jprol.libs.JProlCoreLibrary;
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
    JProlContext context = makeTestContext();
    JProlChoicePoint cp = context.makeChoicePoint("X = 1,  to_json(json([a=1,b=2,c=3]), X).");
    assertNull(cp.prove());
  }

  @Test
  void test_FromJson_noUnify() {
    JProlContext context = prepareContext("", new JProlJsonLibrary());
    JProlChoicePoint cp = context.makeChoicePoint("X = 1,  from_json('{\"hello\":\"world\"}', X).");
    assertNull(cp.prove());
  }

  @Test
  void test_ToJson_errorData() {
    assertToJsonException("[1,2,3]", ProlDomainErrorException.class);
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
    assertFromJson("{\"a\":[1,2,-3,{\"h\":[3,2,4]},[]],\"b\":[{\"ss\":[33.2,44.11,32323]}]}",
        "json([a=[1,2,-3,json([h=[3,2,4]]),[]],b=[json([ss=[33.2,44.11,32323]])]])");
    assertFromJson("{\"hello\":\"w\\\"sss\\u0020\"}",
        "json([hello='w\\\"sss '])");
    assertFromJson("{\n" +
        "  \"name\": \"John Doe\",\n" +
        "  \"age\": 30,\n" +
        "  \"email\": \"john.doe@example.com\",\n" +
        "  \"address\": {\n" +
        "    \"street\": \"123 Main St\",\n" +
        "    \"city\": \"New York\",\n" +
        "    \"country\": \"United States\"\n" +
        "  },\n" +
        "  \"phone_numbers\": [\n" +
        "    \"+1 (555) 123-4567\",\n" +
        "    \"+1 (555) 987-6543\"\n" +
        "  ],\n" +
        "  \"favorite_quotes\": [\n" +
        "    \"Life is what happens\\nwhen you're busy making other plans.\",\n" +
        "    \"The only way to do great work\\nis to love what you do.\"\n" +
        "  ],\n" +
        "  \"bank_balance\": 15000.25\n" +
        "}\n", "json([" +
        "name='John Doe'," +
        "age=30," +
        "email='john.doe@example.com'," +
        "address=json([" +
        "street='123 Main St',city='New York',country='United States'" +
        "])," +
        "phone_numbers=['+1 (555) 123-4567','+1 (555) 987-6543']," +
        "favorite_quotes=['Life is what happens\\nwhen you\\'re busy making other plans.'," +
        "'The only way to do great work\\nis to love what you do.']," +
        "bank_balance=15000.25" +
        "])");

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
    final JProlContext context = makeTestContext();
    final JProlChoicePoint cp = context.makeChoicePoint(String.format("to_json(%s, X).", term));
    Assertions.assertThrowsExactly(expectedException, () -> {
      cp.prove();
      fail("Must throw exception");
    });
  }

  private void assertFromJsonException(final String json,
                                       final Class<? extends Throwable> expectedException) {
    final JProlContext context = makeTestContext();
    final JProlChoicePoint cp = context.makeChoicePoint(String.format("from_json('%s', X).", json));
    Assertions.assertThrowsExactly(expectedException, () -> {
      cp.prove();
      fail("Must throw exception");
    });
  }

  private void assertToJson(final String term, final String expectedJson) {
    JProlContext context = makeTestContext();
    JProlChoicePoint cp = context.makeChoicePoint(String.format("to_json(%s, X).", term));
    assertNotNull(cp.prove());
    assertEquals(
        expectedJson,
        cp.findVar("X").get().getValue().getText());
  }

  private void assertFromJson(final String json, final String expectedTerm) {
    JProlContext context = makeTestContext();
    final String prepared = String.format("from_json('%s', X), X=%s .",
        escapeSrc(json), expectedTerm);
    JProlChoicePoint cp = context.makeChoicePoint(prepared);
    assertNotNull(cp.prove(), "Can't prove goal or not expected value");
  }

  private JProlContext makeTestContext() {
    return this.prepareContext("", new JProlCoreLibrary(), new JProlJsonLibrary());
  }
}