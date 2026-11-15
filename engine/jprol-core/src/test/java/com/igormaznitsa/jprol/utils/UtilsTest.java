package com.igormaznitsa.jprol.utils;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.igormaznitsa.jprol.data.TermList;
import com.igormaznitsa.jprol.data.Terms;
import java.time.Duration;
import java.util.Optional;
import org.junit.jupiter.api.Test;

class UtilsTest {

  @Test
  void testSignatureParsingNormalizationAndReassembly() {
    final ProlPair<String, Integer> simple = ProlUtils.parseSignaturePair("append/3");
    assertEquals("append", simple.getLeft());
    assertEquals(3, simple.getRight());

    final ProlPair<String, Integer> quotedSlash = ProlUtils.parseSignaturePair("'/'/2");
    assertEquals("/", quotedSlash.getLeft());
    assertEquals(2, quotedSlash.getRight());

    assertEquals("foo/2", ProlUtils.normalizeSignature("'foo'/2"));
    assertEquals("foo/2", ProlUtils.normalizeSignature("  foo/2 "));
    assertNull(ProlUtils.normalizeSignature("'unclosed/2"));

    assertEquals("valid_pred/4", ProlUtils.reassembleSignatureOrNull("valid_pred/4"));
    assertEquals("'UpperQuoted'/1", ProlUtils.reassembleSignatureOrNull("'UpperQuoted'/1"));
    assertNull(ProlUtils.reassembleSignatureOrNull("UpperUnquoted/1"));
    assertNull(ProlUtils.reassembleSignatureOrNull("9digit/1"));
    assertNull(ProlUtils.reassembleSignatureOrNull("missing_arity"));

    assertThrows(IllegalArgumentException.class, () -> ProlUtils.parseSignaturePair("no_arity"));
    assertThrows(NumberFormatException.class, () -> ProlUtils.parseSignaturePair("bad/-1"));
    assertThrows(NumberFormatException.class, () -> ProlUtils.parseSignaturePair("bad/xyz"));
  }

  @Test
  void testCharCodeListConversionsAndSourceEscaping() {
    final TermList codes = ProlUtils.toCharCodeList(Terms.newAtom("Hi!"));
    assertEquals("[72,105,33]", codes.toSrcString());
    assertEquals(Optional.of("Hi!"), ProlUtils.fromCharCodeList(codes));
    assertEquals(Optional.of(""), ProlUtils.fromCharCodeList(TermList.NULL_LIST));

    final TermList mixedInvalid = TermList.listOf(Terms.newAtom("not_single_char"));
    assertTrue(ProlUtils.fromCharCodeList(mixedInvalid).isEmpty());

    assertEquals("a\\\\b\\'c\\\"d\\ne\\tf", ProlUtils.escapeSrc("a\\b'c\"d\ne\tf"));
    assertEquals(
        "01:02:03.004",
        ProlUtils.asTimeString(
            Duration.ofHours(1).plusMinutes(2).plusSeconds(3).plusMillis(4)
        )
    );
  }
}
