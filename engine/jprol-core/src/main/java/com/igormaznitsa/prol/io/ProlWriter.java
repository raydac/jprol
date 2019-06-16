package com.igormaznitsa.prol.io;

import com.igormaznitsa.prol.data.Term;

import java.io.IOException;
import java.io.Writer;

public class ProlWriter extends ProlIoResource<Writer> {

  public ProlWriter(final String id, final Writer writer) {
    super(id, writer);
  }

  public void write(final String text) throws IOException {
    this.resource.append(text);
  }

  public void write(final Term term) throws IOException {
    this.resource.append(term.toSrcString());
  }

}
