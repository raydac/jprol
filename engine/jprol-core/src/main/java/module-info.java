open module igormaznitsa.jprol.core {

  exports com.igormaznitsa.jprol.annotations;
  exports com.igormaznitsa.jprol.data;
  exports com.igormaznitsa.jprol.exceptions;
  exports com.igormaznitsa.jprol.kbase;
  exports com.igormaznitsa.jprol.kbase.inmemory;
  exports com.igormaznitsa.jprol.kbase.inmemory.items;
  exports com.igormaznitsa.jprol.libs;
  exports com.igormaznitsa.jprol.logic;
  exports com.igormaznitsa.jprol.logic.io;
  exports com.igormaznitsa.jprol.logic.triggers;
  exports com.igormaznitsa.jprol.trace;
  exports com.igormaznitsa.jprol.utils;
  exports com.igormaznitsa.jprol.utils.lazy;

  requires transitive igormaznitsa.prolog.parser;
}