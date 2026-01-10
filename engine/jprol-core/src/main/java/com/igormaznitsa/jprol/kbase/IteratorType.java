package com.igormaznitsa.jprol.kbase;

public enum IteratorType {
  /**
   * Find only facts. A fact has not tail and its head is grounded.
   */
  FACTS,
  /**
   * Find only rules. A rule has tail or not grounded head.
   */
  RULES,
  /**
   * Find for any type.
   */
  ANY
}
