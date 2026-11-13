package com.igormaznitsa.jprol.iso;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import com.igormaznitsa.jprol.it.AbstractJProlTest;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;
import java.io.IOException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;

@Timeout(30)
class JProlIsoTest extends AbstractJProlTest {

  @Test
  void testIsoArith() throws IOException {
    this.assertIsoSuitePasses("iso_arith_test.prl", 17);
  }

  @Test
  void testIsoAtomNumber() throws IOException {
    this.assertIsoSuitePasses("iso_atom_number_test.prl", 10);
  }

  @Test
  void testIsoCut() throws IOException {
    this.assertIsoSuitePasses("iso_cut_test.prl", 10);
  }

  @Test
  void testIsoFindallSetofBagof() throws IOException {
    this.assertIsoSuitePasses("iso_findall_setof_bagof_test.prl", 7);
  }

  @Test
  void testIsoLength() throws IOException {
    this.assertIsoSuitePasses("iso_length_test.prl", 15);
  }

  @Test
  void testIsoList() throws IOException {
    this.assertIsoSuitePasses("iso_list_test.prl", 17);
  }

  @Test
  void testIsoOrder() throws IOException {
    this.assertIsoSuitePasses("iso_order_test.prl", 64);
  }

  @Test
  void testIsoOrder2() throws IOException {
    this.assertIsoSuitePasses("iso_order_test2.prl", 58);
  }

  @Test
  void testIsoRetract() throws IOException {
    this.assertIsoSuitePasses("iso_retract_test.prl", 76);
  }

  @Test
  void testIsoSyntaxAndOperators() throws IOException {
    this.assertIsoSuitePasses("iso_test.prl", 12);
  }

  @Test
  void testIsoFamilyAndAlgorithms() throws IOException {
    this.assertIsoSuitePasses("iso_test2.prl", 13);
  }

  @Test
  void testIsoIdentity() throws IOException {
    this.assertIsoSuitePasses("iso_test_identity.prl", 78);
  }

  @Test
  void testZeroArgSuiteRunner() throws IOException {
    try (final JProlContext context = this.makeContextAndConsultFromResource("iso_cut_test.prl")) {
      this.assertZeroArgRunnerSucceedsDeterministically(context, "iso_cut_test.prl");
    }
  }

  private void assertIsoSuitePasses(final String resourceName, final int expectedTotal)
      throws IOException {
    try (final JProlContext context = this.makeContextAndConsultFromResource(resourceName)) {
      final IsoSuiteResult result = this.proveIsoSuiteSummary(context, resourceName);
      this.assertSuiteSummary(resourceName, expectedTotal, result);
    }
  }

  private IsoSuiteResult proveIsoSuiteSummary(
      final JProlContext context,
      final String resourceName
  ) {
    final JProlChoicePoint choicePoint =
        context.makeChoicePoint("run_tests(Total, Passed, Failures).");
    assertNotNull(choicePoint.prove(), () -> "Suite goal failed in " + resourceName);

    final IsoSuiteResult result = new IsoSuiteResult(
        getVarAsNumber(choicePoint, "Total").intValue(),
        getVarAsNumber(choicePoint, "Passed").intValue(),
        getVarAsText(choicePoint, "Failures")
    );

    assertNull(choicePoint.prove(), () -> "Unexpected extra solution in " + resourceName);
    return result;
  }

  private void assertSuiteSummary(
      final String resourceName,
      final int expectedTotal,
      final IsoSuiteResult result
  ) {
    assertEquals("[]", result.getFailures(), () -> "Failed checks in " + resourceName);
    assertEquals(expectedTotal, result.getTotal(),
        () -> "Unexpected check count in " + resourceName);
    assertEquals(expectedTotal, result.getPassed(),
        () -> "Passed count mismatch in " + resourceName);
  }

  private void assertZeroArgRunnerSucceedsDeterministically(
      final JProlContext context,
      final String resourceName
  ) {
    final JProlChoicePoint choicePoint = context.makeChoicePoint("run_tests.");
    assertNotNull(choicePoint.prove(), () -> "run_tests/0 failed in " + resourceName);
    assertNull(choicePoint.prove(), () -> "run_tests/0 had extra solution in " + resourceName);
  }

  private static final class IsoSuiteResult {

    private final int total;
    private final int passed;
    private final String failures;

    private IsoSuiteResult(final int total, final int passed, final String failures) {
      this.total = total;
      this.passed = passed;
      this.failures = failures;
    }

    private int getTotal() {
      return this.total;
    }

    private int getPassed() {
      return this.passed;
    }

    private String getFailures() {
      return this.failures;
    }
  }
}
