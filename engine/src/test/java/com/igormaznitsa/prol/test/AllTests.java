package com.igormaznitsa.prol.test;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

@RunWith(Suite.class)
@Suite.SuiteClasses({
        com.igormaznitsa.prol.test.SomeFromISOTest.class,
        com.igormaznitsa.prol.test.PrimitiveTest.class,
        com.igormaznitsa.prol.test.MiscAlgorithms.class,
        com.igormaznitsa.prol.test.OperatorTest.class,
        com.igormaznitsa.prol.test.TriggerTest.class,
        com.igormaznitsa.prol.test.LibraryWrapperTest.class,
        com.igormaznitsa.prol.test.ListTest.class,
        com.igormaznitsa.prol.test.PreparedGoalTest.class,
        com.igormaznitsa.prol.test.IOPipeMemoryTest.class,
        com.igormaznitsa.prol.test.NonDeterministicAutomata.class,
        com.igormaznitsa.prol.test.EightQueens.class,
        com.igormaznitsa.prol.test.HanoiTowers.class,
        com.igormaznitsa.prol.test.PuzzleTest.class,
        com.igormaznitsa.prol.test.EinsteinTest.class,
        com.igormaznitsa.prol.test.StrongTest.class
})
public class AllTests {
}
