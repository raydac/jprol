package com.igormaznitsa.jprol.logic.io;

import com.igormaznitsa.jprol.logic.JProlContext;

import java.io.Reader;
import java.io.Writer;

public interface IoResourceProvider {
  default Reader findReader(JProlContext context, String readerId) {
    return null;
  }

  default Writer findWriter(JProlContext context, String writerId, boolean append) {
    return null;
  }

}
