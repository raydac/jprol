package com.igormaznitsa.prol.logic.io;

import com.igormaznitsa.prol.logic.ProlContext;

import java.io.Reader;
import java.io.Writer;

public interface IoResourceProvider {
  default Reader findReader(ProlContext context, String readerId) {
    return null;
  }

  default Writer findWriter(ProlContext context, String writerId, boolean append) {
    return null;
  }

}
