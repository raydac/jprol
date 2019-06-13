package com.igormaznitsa.prol.test;

import com.igormaznitsa.prol.data.NumericTerm;
import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.logic.ChoicePoint;
import com.igormaznitsa.prol.logic.ProlContext;
import com.igormaznitsa.prol.logic.triggers.AbstractProlTrigger;
import com.igormaznitsa.prol.logic.triggers.ProlTriggerType;
import com.igormaznitsa.prol.logic.triggers.TriggerEvent;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

public class TriggerTest extends AbstractProlTest {

    public TriggerTest() {
    }

    @Test
    public void testTrigger() {
        try {
            final insideTrigger trigger = new insideTrigger();
            trigger.addSignature("testassert/1", ProlTriggerType.TRIGGER_ASSERT);
            trigger.addSignature("testretract/1", ProlTriggerType.TRIGGER_RETRACT);
            trigger.addSignature("testboth/1", ProlTriggerType.TRIGGER_ASSERT_RETRACT);

            ProlContext context = new ProlContext("TestContext");

            context.registerTrigger(trigger);

//            ProlConsult consult = new ProlConsult(testa, context);
//            consult.consult();
          ChoicePoint goal = new ChoicePoint("assert(testassert(1000)),asserta(testassert(1)),assertz(testassert(2)).", context);

            int decisionnum = 0;
            while (true) {
              if (goal.next() == null) {
                    break;
                }
                decisionnum++;
            }

            assertEquals(decisionnum, 1);

          goal = new ChoicePoint("retracta(testassert(_)),retractz(testassert(_)),testassert(X),assert(testretract(test)),retract(testretract(_)),assert(testretract(world)),abolish(testretract/1).", context);

            int result = -1;

            decisionnum = 0;
            while (true) {
              final Term resultterm = goal.next();
                if (resultterm == null) {
                    break;
                }

              result = ((NumericTerm) resultterm.variables().filter(x -> "X".equals(x.getText())).findFirst().orElse(null).getValue()).toNumber().intValue();

                decisionnum++;
            }

            assertEquals(result, 1000);

            int assertevents = 0;
            int retractevents = 0;
            int assertretractevents = 0;

            for (TriggerEvent event : trigger.getBuffer()) {
                switch (event.getEventType()) {
                    case TRIGGER_ASSERT: {
                        assertevents++;
                    }
                    break;
                    case TRIGGER_RETRACT: {
                        retractevents++;
                    }
                    break;
                    case TRIGGER_ASSERT_RETRACT: {
                        assertretractevents++;
                    }
                    break;
                    default: {
                        fail("Unsupported event type");
                        return;
                    }
                }
            }

            assertEquals(assertevents, 3);
            assertEquals(retractevents, 2);
            assertEquals(assertretractevents, 0);

            trigger.getBuffer().clear();

            decisionnum = 0;

          goal = new ChoicePoint("assert(testboth(111)),assert(testboth(222)),testboth(222),retractall(testboth(_)).", context);
            while (true) {
              final Term resultterm = goal.next();
                if (resultterm == null) {
                    break;
                }
                decisionnum++;
            }

            assertEquals(decisionnum, 1);

            assertevents = 0;
            retractevents = 0;
            assertretractevents = 0;

            for (TriggerEvent event : trigger.getBuffer()) {
                switch (event.getEventType()) {
                    case TRIGGER_ASSERT: {
                        assertevents++;
                    }
                    break;
                    case TRIGGER_RETRACT: {
                        retractevents++;
                    }
                    break;
                    case TRIGGER_ASSERT_RETRACT: {
                        assertretractevents++;
                    }
                    break;
                    default: {
                        fail("Unsupported event type");
                        return;
                    }
                }
            }

            assertEquals(assertevents, 2);
            assertEquals(retractevents, 1);

          context.dispose();

            assertEquals(trigger.haltCounter, 1);

        } catch (Exception ex) {
            ex.printStackTrace();
            fail();
        }
    }

    private static class insideTrigger extends AbstractProlTrigger {

        private final List<TriggerEvent> buffer = new ArrayList<>();
        private volatile int haltCounter = 0;

        public insideTrigger() {
            super();
        }

        public int getHaltCounter() {
            return haltCounter;
        }

        public List<TriggerEvent> getBuffer() {
            return buffer;
        }

        @Override
        public void onTriggerEvent(TriggerEvent event) {
            buffer.add(event);
            System.out.println("Event: " + event);
        }

        @Override
        public void onContextHalting(ProlContext context) {
            haltCounter++;
        }
    }
}
