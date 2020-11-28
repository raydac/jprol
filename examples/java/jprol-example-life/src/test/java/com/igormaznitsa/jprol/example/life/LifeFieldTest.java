package com.igormaznitsa.jprol.example.life;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;


import org.junit.jupiter.api.Test;

public class LifeFieldTest {

  @Test
  public void testNegativeCoords() {
    final LifeField container = new LifeField();
    assertFalse(container.get(-5, -1034));
    container.set(-5, -1034, true);
    assertTrue(container.get(-5, -1034));
    container.set(-5, -1034, false);
    assertFalse(container.get(-5, -1034));
  }

  @Test
  public void testPositiveCoords() {
    final LifeField container = new LifeField();
    assertFalse(container.get(5824, 3345));
    container.set(5824, 3345, true);
    assertTrue(container.get(5824, 3345));
    container.set(5824, 3345, false);
    assertFalse(container.get(5824, 3345));
  }

}
