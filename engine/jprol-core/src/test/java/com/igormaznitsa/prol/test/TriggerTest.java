package com.igormaznitsa.prol.test;

import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.logic.ChoicePoint;
import com.igormaznitsa.prol.logic.ProlContext;
import com.igormaznitsa.prol.logic.triggers.AbstractProlTrigger;
import com.igormaznitsa.prol.logic.triggers.ProlTriggerType;
import com.igormaznitsa.prol.logic.triggers.TriggerEvent;
import org.junit.jupiter.api.Test;

import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;

class TriggerTest extends AbstractProlTest {

  @Test
  void testTrigger() throws Exception {
    final InternaltestTrigger trigger = new InternaltestTrigger();
    trigger.addSignature("testassert/1", ProlTriggerType.TRIGGER_ASSERT);
    trigger.addSignature("testretract/1", ProlTriggerType.TRIGGER_RETRACT);
    trigger.addSignature("testboth/1", ProlTriggerType.TRIGGER_ASSERT_RETRACT);

    ProlContext context = new ProlContext("TestContext");

    context.registerTrigger(trigger);

//            ProlConsult consult = new ProlConsult(testa, context);
//            consult.consult();
    ChoicePoint goal = new ChoicePoint("assert(testassert(1000)),asserta(testassert(1)),assertz(testassert(2)).", context);

    int decisionnum = 0;
    while (goal.next() != null) {
      decisionnum++;
    }

    assertEquals(1, decisionnum);

    goal = new ChoicePoint("retracta(testassert(_)),retractz(testassert(_)),testassert(X),assert(testretract(test)),retract(testretract(_)),assert(testretract(world)),abolish(testretract/1).", context);

    int result = -1;

    decisionnum = 0;
    Term resultterm;
    while ((resultterm = goal.next()) != null) {
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
    goal = new ChoicePoint("assert(testboth(111)),assert(testboth(222)),testboth(222),retractall(testboth(_)).", context);
    while (goal.next() != null) {
      decisionnum++;
    }

    assertEquals(1, decisionnum);

    assertEquals(2, trigger.assertevents.get());
    assertEquals(1, trigger.retractevents.get());
    assertEquals(0, trigger.assertretractevents.get());

    context.dispose();

    assertEquals(1, trigger.haltCounter.get());
  }

  private static class InternaltestTrigger extends AbstractProlTrigger {

    private final AtomicInteger haltCounter = new AtomicInteger();
    private final AtomicInteger assertevents = new AtomicInteger();
    private final AtomicInteger retractevents = new AtomicInteger();
    private final AtomicInteger assertretractevents = new AtomicInteger();

    InternaltestTrigger() {
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
    public void onContextHalting(ProlContext context) {
      haltCounter.incrementAndGet();
    }
  }
}
