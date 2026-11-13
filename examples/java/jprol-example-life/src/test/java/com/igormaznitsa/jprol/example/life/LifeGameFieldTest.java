package com.igormaznitsa.jprol.example.life;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

public class LifeGameFieldTest {

  @Test
  public void testNegativeCoords() {
    final LifeGameField container = new LifeGameField();
    assertFalse(container.get(-5, -1034));
    container.set(-5, -1034, true);
    assertTrue(container.get(-5, -1034));
    container.set(-5, -1034, false);
    assertFalse(container.get(-5, -1034));
  }

  @Test
  public void testPositiveCoords() {
    final LifeGameField container = new LifeGameField();
    assertFalse(container.get(5824, 3345));
    container.set(5824, 3345, true);
    assertTrue(container.get(5824, 3345));
    container.set(5824, 3345, false);
    assertFalse(container.get(5824, 3345));
  }

  @Test
  public void testBlinkPublishesNextAndClearsIt() {
    final LifeGameField container = new LifeGameField();
    container.set(1, 1, true);
    container.setNext(2, 2, true);

    container.blinkGeneration();
    assertTrue(container.get(2, 2));
    assertFalse(container.get(1, 1));

    container.blinkGeneration();
    assertFalse(container.get(1, 1));
    assertFalse(container.get(2, 2));
  }

}
