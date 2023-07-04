package com.igormaznitsa.jprol.libs;

import com.igormaznitsa.jprol.logic.JProlContext;
import java.io.StringReader;

public abstract class AbstractJProlTest {
  protected JProlContext prepareContext(final String consult,
                                        final AbstractJProlLibrary... libraries) {
    final JProlContext context = new JProlContext("test", libraries);
    context.consult(new StringReader(consult));
    return context;
  }
}
