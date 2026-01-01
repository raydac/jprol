package com.igormaznitsa.jprol.it;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.fail;

import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;
import com.igormaznitsa.jprol.logic.triggers.AbstractJProlContextTrigger;
import com.igormaznitsa.jprol.logic.triggers.JProlTriggerType;
import com.igormaznitsa.jprol.logic.triggers.TriggerEvent;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import org.junit.jupiter.api.Test;

class TriggerTest extends AbstractJProlTest {

  private TestContextTriggerCallCounter makeCounter() {
    final TestContextTriggerCallCounter trigger = new TestContextTriggerCallCounter();
    trigger.register("testassert/1", EnumSet.of(JProlTriggerType.TRIGGER_ASSERT));
    trigger.register("testretract/1", EnumSet.of(JProlTriggerType.TRIGGER_RETRACT));
    trigger.register("testassertretract/1",
        EnumSet.of(JProlTriggerType.TRIGGER_ASSERT, JProlTriggerType.TRIGGER_RETRACT));
    trigger.register("testabolish/1", EnumSet.of(JProlTriggerType.TRIGGER_ABOLISH));
    return trigger;
  }

  private JProlContext makeContext() {
    return makeTestContext(
        ":- dynamic [testassert/1,testretract/1,testassertretract/1,testabolish/1].");
  }

  private void execute(final String goal, final TestContextTriggerCallCounter counter) {
    try (final JProlContext context = makeContext()) {
      context.addTrigger(counter);
      final JProlChoicePoint choicePoint = context.makeChoicePoint(goal);
      assertNotNull(choicePoint.prove());
      assertNull(choicePoint.prove());
    }
    assertEquals(1, counter.haltCounter.get());
  }

  @Test
  void testTriggersAssertRetract1() {
    final TestContextTriggerCallCounter counter = makeCounter();
    execute(
        "asserta(testassertretract(0)),assert(testassert(_)), asserta(testassert(1)), assertz(testassert(2))," +
            "retract(testassert(2)),retractz(testassert(1)),retracta(testassertretract(0))."
        , counter);

    assertEquals(0, counter.findDetected(JProlTriggerType.TRIGGER_ABOLISH));
    assertEquals(2, counter.findDetected(JProlTriggerType.TRIGGER_ASSERT));
    assertEquals(1, counter.findDetected(JProlTriggerType.TRIGGER_RETRACT));

    assertEquals(1, counter.findCounter(JProlTriggerType.TRIGGER_ASSERT, "testassertretract/1"));
    assertEquals(3, counter.findCounter(JProlTriggerType.TRIGGER_ASSERT, "testassert/1"));
    assertEquals(1, counter.findCounter(JProlTriggerType.TRIGGER_RETRACT, "testassertretract/1"));
    assertEquals(0, counter.findCounter(JProlTriggerType.TRIGGER_ABOLISH, "testabolish/1"));
  }

  @Test
  void testTriggersAssertRetract2() {
    final TestContextTriggerCallCounter counter = makeCounter();
    execute(
        "asserta(testassertretract(0)),"
            + "assert(testretract(0)),"
            + "asserta(testretract(1)),"
            + "assertz(testretract(2)),"
            + "retract(testretract(2)),"
            + "retractz(testretract(1)),"
            + "retracta(testassertretract(0))."
        , counter);

    assertEquals(0, counter.findDetected(JProlTriggerType.TRIGGER_ABOLISH));
    assertEquals(1, counter.findDetected(JProlTriggerType.TRIGGER_ASSERT));
    assertEquals(2, counter.findDetected(JProlTriggerType.TRIGGER_RETRACT));

    assertEquals(1, counter.findCounter(JProlTriggerType.TRIGGER_ASSERT, "testassertretract/1"));
    assertEquals(2, counter.findCounter(JProlTriggerType.TRIGGER_RETRACT, "testretract/1"));
    assertEquals(0, counter.findCounter(JProlTriggerType.TRIGGER_ABOLISH, "testabolish/1"));
  }

  @Test
  void testTriggersAbolish() {
    final TestContextTriggerCallCounter counter = makeCounter();
    execute(
        "asserta(testassertretract(0)),"
            + "assert(testretract(0)),"
            + "asserta(testretract(1)),"
            + "assertz(testretract(2)),"
            + " asserta(testabolish(1)),"
            + "retract(testretract(2)),"
            + "retractz(testretract(1)),"
            + "retracta(testassertretract(0)),"
            + "abolish(testabolish/1)."
        , counter);

    assertEquals(1, counter.findDetected(JProlTriggerType.TRIGGER_ABOLISH));
    assertEquals(1, counter.findDetected(JProlTriggerType.TRIGGER_ASSERT));
    assertEquals(2, counter.findDetected(JProlTriggerType.TRIGGER_RETRACT));

    assertEquals(1, counter.findCounter(JProlTriggerType.TRIGGER_ASSERT, "testassertretract/1"));
    assertEquals(2, counter.findCounter(JProlTriggerType.TRIGGER_RETRACT, "testretract/1"));
    assertEquals(1, counter.findCounter(JProlTriggerType.TRIGGER_ABOLISH, "testabolish/1"));
  }


  private static class TestContextTriggerCallCounter extends AbstractJProlContextTrigger {

    private final AtomicInteger haltCounter = new AtomicInteger();
    private final Map<String, AtomicInteger> assertCounter = new HashMap<>();
    private final Map<String, AtomicInteger> retractCounter = new HashMap<>();
    private final Map<String, AtomicInteger> abolishCounter = new HashMap<>();

    TestContextTriggerCallCounter() {
      super();
    }

    public int findDetected(JProlTriggerType triggerType) {
      switch (triggerType) {
        case TRIGGER_ABOLISH:
          return abolishCounter.size();
        case TRIGGER_ASSERT:
          return assertCounter.size();
        case TRIGGER_RETRACT:
          return retractCounter.size();
        default:
          fail("Unsupported trigger type: " + triggerType);
      }
      throw new Error();
    }

    public int findCounter(JProlTriggerType triggerType, String signature) {
      switch (triggerType) {
        case TRIGGER_ABOLISH:
          return abolishCounter.getOrDefault(signature, new AtomicInteger()).get();
        case TRIGGER_ASSERT:
          return assertCounter.getOrDefault(signature, new AtomicInteger()).get();
        case TRIGGER_RETRACT:
          return retractCounter.getOrDefault(signature, new AtomicInteger()).get();
        default:
          fail("Unsupported trigger type: " + triggerType);
      }
      throw new Error();
    }

    @Override
    public void onTriggerEvent(final TriggerEvent event) {
      System.out.println(event);
      final String signature = event.getSignature();
      switch (event.getEventType()) {
        case TRIGGER_ASSERT:
          assertCounter.computeIfAbsent(signature, k -> new AtomicInteger()).incrementAndGet();
          break;
        case TRIGGER_RETRACT:
          retractCounter.computeIfAbsent(signature, k -> new AtomicInteger()).incrementAndGet();
          break;
        case TRIGGER_ABOLISH:
          abolishCounter.computeIfAbsent(signature, k -> new AtomicInteger()).incrementAndGet();
          break;
        default:
          fail("Unexpected trigger event " + event);
      }
    }

    @Override
    public void onContextDispose(JProlContext context) {
      haltCounter.incrementAndGet();
    }
  }
}
