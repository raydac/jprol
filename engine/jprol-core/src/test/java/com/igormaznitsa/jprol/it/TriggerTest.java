package com.igormaznitsa.jprol.it;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;
import com.igormaznitsa.jprol.logic.triggers.AbstractJProlTrigger;
import com.igormaznitsa.jprol.logic.triggers.JProlTriggerType;
import com.igormaznitsa.jprol.logic.triggers.TriggerEvent;
import java.util.concurrent.atomic.AtomicInteger;
import org.junit.jupiter.api.Test;

class TriggerTest extends AbstractJProlTest {

  @Test
  void testTrigger() {
    final InternalTestTrigger trigger = new InternalTestTrigger();
    trigger.addSignature("testassert/1", JProlTriggerType.TRIGGER_ASSERT);
    trigger.addSignature("testretract/1", JProlTriggerType.TRIGGER_RETRACT);
    trigger.addSignature("testboth/1", JProlTriggerType.TRIGGER_ASSERT_RETRACT);

    JProlContext context = makeTestContext();
    new JProlChoicePoint(":- dynamic([testassert/1,testretract/1,testboth/1]).", context).prove();

    context.registerTrigger(trigger);

    JProlChoicePoint goal = new JProlChoicePoint(
        "assert(testassert(1000))," +
            "asserta(testassert(1))," +
            "assertz(testassert(2))."
        , context);

    int decisionnum = 0;
    while (goal.prove() != null) {
      decisionnum++;
    }

    assertEquals(1, decisionnum);

    goal = new JProlChoicePoint(
        "retracta(testassert(_))," +
            "retractz(testassert(_))," +
            "testassert(X)," +
            "assert(testretract(test))," +
            "retract(testretract(_))," +
            "assert(testretract(world))," +
            "abolish(testretract/1).", context);

    int result = -1;

    decisionnum = 0;
    Term resultterm;
    while ((resultterm = goal.prove()) != null) {
      result = resultterm.variables()
          .filter(x -> "X".equals(x.getText()))
          .findFirst()
          .orElse(null)
          .getValue()
          .toNumber()
          .intValue();
      decisionnum++;
    }

    assertEquals(result, 1000);

    assertEquals(3, trigger.assertevents.get());
    assertEquals(2, trigger.retractevents.get());
    assertEquals(0, trigger.assertretractevents.get());

    trigger.clear();

    decisionnum = 0;
    goal = new JProlChoicePoint("assert(testboth(111)),assert(testboth(222)),testboth(222),retractall(testboth(_)).", context);
    while (goal.prove() != null) {
      decisionnum++;
    }

    assertEquals(1, decisionnum);

    assertEquals(2, trigger.assertevents.get());
    assertEquals(1, trigger.retractevents.get());
    assertEquals(0, trigger.assertretractevents.get());

    context.dispose();

    assertEquals(1, trigger.haltCounter.get());
  }

  private static class InternalTestTrigger extends AbstractJProlTrigger {

    private final AtomicInteger haltCounter = new AtomicInteger();
    private final AtomicInteger assertevents = new AtomicInteger();
    private final AtomicInteger retractevents = new AtomicInteger();
    private final AtomicInteger assertretractevents = new AtomicInteger();

    InternalTestTrigger() {
      super();
    }

    void clear() {
      this.haltCounter.set(0);
      this.assertretractevents.set(0);
      this.assertevents.set(0);
      this.retractevents.set(0);
    }

    @Override
    public void onTriggerEvent(TriggerEvent event) {
      switch (event.getEventType()) {
        case TRIGGER_ASSERT:
          assertevents.incrementAndGet();
          break;
        case TRIGGER_RETRACT:
          retractevents.incrementAndGet();
          break;
        case TRIGGER_ASSERT_RETRACT:
          assertretractevents.incrementAndGet();
          break;
      }
    }

    @Override
    public void onContextHalting(JProlContext context) {
      haltCounter.incrementAndGet();
    }
  }
}
